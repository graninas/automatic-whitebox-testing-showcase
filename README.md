# Automatic White-Box Testing with Free Monads

### Summary

Automatic creation of regression tests by designing a system that records the input, output and side-effects of a business application in production. Functional programming and Free monads, that  separate the pure computation from side-effects, enabled this innovation.

---

Building applications with complex business logic is rarely done without testing. Essentially, the more complex business scenarios you have the easier it is to break something while editing the code. Pure calculations, DB interaction, communication with external services, state mutation - all these code parts may change with time, and sometimes by mistake. Moreover, external services your logic interacts with can also change even when they shouldn’t, and it will immediately make the code invalid. Thus, to be sure that the code works as it should, various testing is needed. But testing requires a lot of labour work, and it’s not really clear whether some kinds of tests are worth it. In this article, we’ll see that there is a very powerful approach to make testing as easy as possible.

**Note.** We’ve developed this approach at [Juspay](https://juspay.in/) as a feature of the framework [Presto.Backend](https://github.com/juspay/purescript-presto-backend/tree/feature/record-replay) (Open Source, Free monad based). We’re already using it in production for our QA needs.

**Note.** This article is not an introduction into Free Monads. You can get additional information in my book ["Functional Design and Architecture"](https://github.com/graninas/Functional-Design-and-Architecture) or try another resources.

**Note.** This showcase project is under development. More features will be added soon.

![Recording-Replaying](https://github.com/graninas/automatic-whitebox-testing-showcase/blob/master/Recording-Replaying.png)

- [Integration tests](#integration-tests)
- [Automatic white-box testing](#Automatic-white-box-testing)
- [Free monad eDSLs for business logic](#Free-monad-eDSLs-for-business-logic)
- [The recording-replaying mechanism: entries and run modes](#The-recording-replaying-mechanism-entries-and-run-modes)
- [The recording-replaying mechanism](#The-recording-replaying-mechanism)
- [Abstracting over the native libraries and types](#Abstracting-over-the-native-libraries-and-types)
- [Presto Backend possibilities and PureScript differences](#Presto-Backend-possibilities-and-PureScript-differences)
- [Conclusion](#Conclusion)
- [Acknowledges](#Acknowledges)

### Integration tests

Integration tests can protect the logic to some degree. These tests consider the system to be a black-box working in the special environment, and interact with this box via public interface to check is the behaviour valid. Integration tests are very close to a production-like running of the code except they probably don’t usually make calls to real services and don’t use a real production data. Instead of this, integration tests may use some special environments like sandboxes and dummy services to simulate an actual interaction.

There is a problem, though. Integration tests can be a bit fragile because the dependent services are not guaranteed to respond all the time. Sometimes integration tests fail by external reason, and this may be or may be not acceptable in different situations. If we want to make the tests completely stable, we need to mock all the external services. Taken to the extreme point, a test that has all the external calls and unpredictable side effects mocked cannot fail by the unexpected reason. Being run with mocks, the logic represents a pure computation which behaviour is definite and straightforward. We can pass a predefined input and get a predefined result from it. If the result somehow differs from the expected one, then the code has changed since the test was written. It’s either a bug or a valid change - we have to double check.

One may argue that tests of this kind (with mocks) have too much in common with white-box unit testing from the mainstream practices. That’s true, the approaches are very similar. The next concern will be that these tests are a footprint of the logic and they are very sensitive to any code change. Creating and managing unit tests for long scenarios is a nightmare because the logic tends to evolve with time and the tests should be updated respectively. The more logic you have, the more management of unit tests you need. Annoying. Painful. Time-consuming.

Now, knowing the title, you might have guessed already that we’re going to get rid of that hard work and make the white-box testing do a good job. Let’s see...

### Automatic white-box testing

So what does this mean? How these tests look like? Well, there are three parts of the automatic testing system:

* Recordings
* Recorder
* Player

Recording is a text file recorded by, well, the recorder. This file contains a step-by-step footprint of a particular business logic scenario. There will be all the information related to the scenario:  input parameters, result, all the effects, all the external calls, all the DB interactions and so on. Every significant step will be represented by a specific entry, and every entry is considered to be a mock for a particular scenario step (see the diagram above).
For example, when a call to a DB happens, there will be recorded an entry for it:

```haskell
getStudentsCount :: Flow Int
getStudentsCount = do
  students <- runDBQuery "SELECT * FROM students"
  when (null students) $ logInfo "No records found."
  pure $ length students
```

```json
// recording.json:
{
    "entries": [
      [
        0, "RunDBQueryEntry",
        "{\"contents\":{\"jsonResult\":\"[]\", \"query\":\"SELECT * FROM students\"}}"
      ],
      [
        1, "LogInfoEntry",
        "{\"contents\":{\"message\":\"No records found.\"}}"
      ]
    ]
}
```

Having a whole scenario evaluation “serialized”, it is possible to “replay” the recording against the scenario again. If there are some changes in the logic, the player will immediately break on a step that differs. It will also show a sane error message describing the step recorded and the step happened, and thus it’s possible to localize the bug for a better debugging.

Here is a sample output on a recording being replayed against the broken scenario:

```haskell
getStudentsCount :: Flow Int
getStudentsCount = do
  -- FIXME: for debug only
  -- students <- runDBQuery "SELECT * FROM students"
  -- when (null students) $ logInfo "No records found."
  -- pure $ length students
  pure 10
```

```
$ player "recording.json"

[FAIL] Playback failed: unexpected flow end. Expected:
{ 0, "RunDBQueryEntry", "{\"jsonResult\":\"[]\",\"contents\": {\"query\": \"SELECT * FROM students\"}}"
```

Notably, full mocking of the effects in the recordings is not the only thing we can do. More interesting use cases of automatic white-box testing emerge if we allow the playback to be configured in several ways.

* **Partial mocking.** It might be useful to define what effects and external calls should be mocked, and what effects should be run. For example, you might want to mock the calls to all HTTP services but prefer to interact with a real DB to test some particular scenarios. With this, you’ll get a variant of integration tests which know a lot about logic. If this “knowledge” is not something desirable, you can configure the system to make it work as a black-box test as much as needed.
* **Selective verification.** By default, every step is checked for match with the recorded one: input parameters, additional info, output result. But sometimes it’s needed to disable such verification for a set of entries while still doing the mocking.
* **Disabled entries.** Sometimes the step should not affect the replaying process at all. For example, it’s possible to disable all the log entries so they won’t be played and checked somehow (the real effects will be evaluated).

For even more advanced situations, a subtle tuning of the replaying process can be done with both global player configs and configs of a specific entry in the recording.

Last but not the least thing to note that all of this can be achieved without affecting the business logic code. There will be no evidence in the logic that it’s recordable and replayable, and therefore no extra complexity will be brought into the project. The only pre requirement to implement such recording-replaying mechanism is to follow the approach with the business logic abstracted by Free monad domain specific language to enable a core manipulation with all the steps the logic has. We’ll be discussing this approach in the rest of the article.

### Free monad eDSLs for business logic

It’s crucial to understand why we need to abstract our business logic with Free monads to enable such option as the automatic white-box testing. It seems other approaches (`Final Tagless`, `Service Handle Pattern`, `ReaderT Pattern`) do not allow to create this recording-replaying mechanism, or it will be less convenient to do so. It’s probably possible with FT to overcome this problem with different additional wrappers, but let’s agree that introspection of Free monads makes this task much easier.

Suppose we have a Free monadic language with the following methods available:

```haskell
data FlowF next where
  GenerateGUID :: (String -> next) -> FlowF next
  RunIO :: IO s -> (s -> next) -> FlowF next
  LogInfo :: String -> (() -> next) -> FlowF next

type Flow a = Free FlowF a

generateGUID :: Flow String
generateGUID = liftF $ GenerateGUID id

runIO :: IO s -> Flow s
runIO ioAct = liftF $ RunIO ioAct id

logInfo :: String -> Flow ()
logInfo msg = liftF $ LogInfo msg id
```

This is a simple eDSL that has only three methods: generating UUID, logging a message and evaluating a random `IO` effect. A toy example of a business logic scenario will be:

```haskell
compareGUIDs :: String -> Flow ()
compareGUIDs fileName = do
  newGuid <- generateGUID
  oldGuid <- runIO $ readFile fileName

  let equal = newGuid == oldGuid
  when equal $ logInfo "GUIDs are equal."
  unless equal $ logInfo "GUIDs are not equal."
```

This program obtains a new GUID, reads a file for getting an old GUID and compares whether these two GUIDs are equal. (It can possibly crash if the readFile function throws an exception, let’s just ignore this for now.) Not very interesting program that is enough for us to talk about why we need this level of abstraction.

Firstly, this Free monadic language is testable. Running the `compareGUIDs` script with different interpreters allows either to perform real effects or mock them for our testing purposes.

```haskell
-- Real interpreter
interpretFlowF :: FlowF a -> IO a
interpretFlowF (GenerateGUID next) = next . toString <$> nextRandom
interpretFlowF (RunIO ioAct next)  = next <$> ioAct
interpretFlowF (LogInfo msg next)  = next <$> putStrLn msg

-- Test mocking interpreter
interpretFlowFTest :: FlowF a -> IO a
interpretFlowFTest (GenerateGUID next) = pure $ next "111"
interpretFlowFTest (RunIO ioAct next)  = error "IO not supported in tests."
interpretFlowFTest (LogInfo msg next)  = pure $ next ()
```

The `RunIO` method makes some troubles here. By the definition, we don’t know what result should be returned, we only know it’s a value of an arbitrary type `a`. It’s not mockable because mocking essentially means substituting a result by some predefined value of this type, and it’s possible only when the type is well-known. So it’s more likely that we won’t be able to test a flow containing such `runIO` method. To avoid the problem, we can at least require the type to be unit, so no return value is expected from the effect, and therefore we can handle it by “doing nothing”:

```haskell
data FlowF next where
  RunIO :: IO () -> (() -> next) -> FlowF next

runIO :: IO () -> Flow ()
runIO ioAct = liftF $ RunIO ioAct id

interpretFlowFTest :: FlowF a -> IO a
interpretFlowFTest (RunIO _ next) = pure $ next ()
```

This is fine unless we do need the results from there. For example, a specific type that came from an external library: database connection, file handle, `IORef`, `MVar` and so on. Let’s consider the following flow with a `DB.Connection` type that came from the external library:

```haskell
import qualified DB.Native as DB

initDB :: String -> DB.Config -> Flow (Maybe DB.Connection)
initDB dbName cfg = do
  mbConn <- runIO $ DB.initDatabase dbName cfg
  when (isJust mbConn) $ logInfo "Successfully initialized."
  pure mbConn
```

Our `RunIO` step cannot be recorded, so it will be absent in the recording. But when the player hits this `runIO` call, it will have to run a real effect, which is not what should happen. This effectively means the usage of bare types is not allowed because all the steps should be written into the recording. How we can solve this problem? To make scenarios recordable and replayable we have to abstract all the bare types by our own mockable and serializable types. We’ll see how to do it with DB connections in the next part of the article, and for now we’ll just proceed with a tiny change in the language. We’ll constrain the `RunIO` method by the `ToJSON / FromJSON` instances from the `aeson` package for the type `a`:

```haskell
data FlowF next where
  RunIO :: (ToJSON s, FromJSON s) => IO s -> (s -> next) -> FlowF next

runIO :: (ToJSON s, FromJSON s) => IO s -> Flow s
runIO ioAct = liftF $ RunIO ioAct id

-- Test mocking interpreter
interpretFlowFTest :: FlowF a -> IO a
interpretFlowFTest (RunIO ioAct next) =
  = pure $ next $ fromJust $ decode "{some_json_obj_here}"
```

Now, our language is ready for the recording-replaying mechanism.

### The recording-replaying mechanism: entries and run modes

The idea behind this mechanism is to have three modes for the interpreter:

* **Regular mode.** The scenario should be interpreted as usual.
* **Recorder mode.** Every language step should be evaluated as usual, but also it should produce an entry describing what happened on this step (input parameters, output result, additional info).
* **Player mode.** The interpreter will receive an array of recording entries, and it will be going through the scenario step-by-step, popping the next entry from the recording and doing a replay. In this mode, no real effect will be evaluated. Instead, entries will be providing mocks for steps, and the scenario will be used as a sequence of the steps that should be replayed.

In the recording-replaying mechanism, all the `Flow` methods should be accompanied with a corresponding entry types, for instance:

```haskell
data GenerateGUIDEntry = GenerateGUIDEntry { guid :: String }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data RunIOEntry = RunIOEntry { jsonResult :: String }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data LogInfoEntry = LogInfoEntry { message :: String }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Smart constructors
mkGenerateGUIDEntry :: String -> GenerateGUIDEntry
mkGenerateGUIDEntry guidStr = GenerateGUIDEntry guidStr

mkRunIOEntry :: ToJSON ioResult => ioResult -> RunIOEntry
mkRunIOEntry = RunIOEntry . encodeToStr

mkLogInfoEntry :: String -> () -> LogInfoEntry
mkLogInfoEntry msg _ = LogInfoEntry msg
```

These types will be serialized and written into the recording file. Sequence of such entries represents a particular scenario - its key steps with effects. Pure calculations won’t appear in the recordings because they are not encoded as Free monadic actions. If you need a pure calculation to be recorded, you can either introduce a method for it or turn this pure calculation into the impure one and pass it to `runIO`. In here, you’ll have to decide how many info about the calculation you want to record: the result only or the arguments and the operation too. You may end up with adding a separate Free language for expressions for a better granularity of your recordings, but that is another story...

Technically, it’s seems clear how the recording mode should work: on every step, push a corresponding entry into the recordings array collecting them all during the evaluation. At the end we’ll have a recording that we may write into the file. Except... How would we put all these different entry types into a homogenous container? Well, we could just have a single type ADT for all the entries like so:

```haskell
data RecordingEntry
  = GenerateGUIDEntry { guid :: String }
  | RunIOEntry { jsonResult :: String }
  | LogInfoEntry { message :: String }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
```

This is fine, but let’s make our lives harder. We’ll just encode an entry type and put it as string into the following `RecordingEntry` type:

```haskell
type EntryIndex = Int
type EntryName = String
type EntryPayload = String

data RecordingEntry = RecordingEntry EntryIndex EntryName EntryPayload
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

type RecordingEntries = IntMap RecordingEntry
newtype Recording = Recording RecordingEntries
```

This is an operational data that exists only on the runtime layer, and it should not anyhow appear on the business logic layer. So we want to maintain a structure for runtime and interpreting process:

```haskell
data RecorderRuntime = RecorderRuntime
  { recordingRef :: MVar RecordingEntries
  }
```

For the player mode, we’ll put the recordings and the current step into the PlayerRuntime. There will also be a variable for a playback error that could have possibly happened. Here:

```haskell
data PlayerRuntime = PlayerRuntime
  { recording :: RecordingEntries
  , stepRef :: MVar Int
  , errorRef :: MVar PlaybackError
  }
```

The interpreter works in either of three modes. There can be another runtime operational data such as current DB connections, options, variables, that is needed by the interpreter. The `Runtime` type is a good place to keep this data (if you don’t want to use `ReaderT` or `StateT`):

```haskell
data Runtime = Runtime
  { runMode :: RunMode
    -- More operational data for the interpreter
  }

data RunMode
  = RegularMode
  | RecordingMode RecorderRuntime
  | ReplayingMode PlayerRuntime

interpretFlowF :: Runtime -> FlowF a -> IO a
interpretFlowF rt … = …

runFlow :: Runtime -> Flow a -> IO a
runFlow rt = foldFree (interpretFlowF rt)
```

So we prepared the environment for our Free monad language. Now, the most interesting part goes here: the details of the recording-replaying mechanism itself.

### The recording-replaying mechanism

The recording mode is pretty simple and boring: the interpreter just walks by the script step-by-step and produces the entries. The replaying mode is a bit more difficult. In it, the interpreter also does a step-by-step interpreting, but the replaying mechanism should track entries from the recording and match them with the current step. It might be the script has changed and the entry won’t match the step: either its type, or the input parameters stored in it. When this happens, the replaying will be failed.

There are more reasons for the playback failure while the player works. This is a happy path:

* Take the flow step.
* Take the next recording entry from the recording.
* Decode the recording entry.
* Check step and entry for match.
* Check input parameters for match.
* Decode a mock value.
* Do not evaluate the real effect but rather return the mock value instead.

Different errors can happen all the way down, and the player will finish with a playback error:

```haskell
data PlaybackErrorType
  = UnexpectedRecordingEnd
  | UnknownRRItem
  | MockDecodingFailed
  | ItemMismatch

data PlaybackError = PlaybackError
  { errorType :: PlaybackErrorType
  , errorMessage :: String
  }
```

The error message (and the diff between the previous and the current flows) is usually enough to understand what happened. There will be a step index, an entry and the current `FlowF` method description. On a closer look however it’s not so obvious how the player obtains this info. Let’s elaborate that.

When the interpreter hits a particular method, the latter contains all the information about the step. For example, the `LogInfo` method carries the message string, the `RunIO` method has a return type defined and so on. The replaying mechanism should be able to decode mocks, to prepare an entry for serialization, to check the input parameters of the method (if there are such parameters). We’re passing this information into the mechanism by associating it with the corresponding entry using the two type classes. First of them, `RRItem`, allows to serialize and deserialize the entry:

```haskell
class (Eq rrItem, ToJSON rrItem, FromJSON rrItem)
  => RRItem rrItem where
  toRecordingEntry   :: rrItem -> Int -> RecordingEntry
  fromRecordingEntry :: RecordingEntry -> Maybe rrItem
  getTag             :: Proxy rrItem -> String

instance RRItem GenerateGUIDEntry where
  toRecordingEntry rrItem idx = …
  fromRecordingEntry re = …
  getTag _ = "GenerateGUIDEntry"
```

The second type class, `MockedResult`, allows to extract a mock value from the entry:

```haskell
class RRItem rrItem => MockedResult rrItem native where
  getMock :: rrItem -> Maybe native

instance MockedResult GenerateGUIDEntry String where
  getMock (GenerateGUIDEntry g) = Just g
```

Notice that the native type is not necessarily serializable, it’s just something we should return from the `getMock` function. We are free to store some another type into the entry. This is a subtle design detail, though.

Let’s move forward. There is a `withRunMode` function that is the entry point for the mechanism. It takes the run mode, the native effect (to be or not to be evaluated), and the entry creation function:

```haskell
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)

interpretFlowF (Runtime mode) (GenerateGUID next) = do
  let (eff :: IO String) = toString <$> nextRandom
  guidStr <- withRunMode mode mkGenerateGUIDEntry eff
  pure $ next guidStr
```

Note that we don’t evaluate the effect immediately here, this is why we can’t finish construction of the entry: the result of the effect does not yet exist. So the final entry will be formed later using this construction function.

Here is the interpreter for all methods we have. It’s written a bit more concise (and probably, less clear):

```haskell
interpretFlowF :: Runtime -> FlowF a -> IO a

interpretFlowF (Runtime mode) (GenerateGUID next) =
  next <$> withRunMode mode mkGenerateGUIDEntry (toString <$> nextRandom)

interpretFlowF (Runtime mode) (RunIO ioAct next) =
  next <$> withRunMode mode mkRunIOEntry ioAct

interpretFlowF (Runtime mode) (LogInfo msg next) =
  next <$> withRunMode mode (mkLogInfoEntry msg) (putStrLn msg)
```

So, what’s inside the `withRunMode` function? Well, it’s just a switch for the mode. All the underlying functions work with entries abstracted by the type classes.

```haskell
withRunMode :: RRItem rrItem => MockedResult rrItem native
  => RunMode -> (native -> rrItem) -> IO native -> IO native

withRunMode RegularMode _ act = act

withRunMode (RecordingMode recorderRt) mkRRItem act
  = record recorderRt Proxy mkRRItem act

withRunMode (ReplayingMode playerRt) mkRRItem act
  = replay playerRt mkRRItem act
```

Going deeper to the implementation seems not that necessary for this storytelling. The `record` and `replay` functions store and load entries, decode results, make checks and verifications. A more developed mechanism also supports configs for replaying and recording. You can see how it’s [done](https://github.com/graninas/automatic-whitebox-testing-showcase/blob/master/src/Playback/Machine.hs) in the showcase project, and now we’d better cover an important question we mentioned earlier. Let’s return to the design space and talk about why we have to abstract native types and libraries for this mechanism particularly and in general.

### Abstracting over the native libraries and types

The problem is that we cannot operate by the types that are not serializable because this immediately makes the scenario unrecordable and unreplayable. Why so? Back to the samples with database interaction, we might want to use a native connection type in the flows:

```haskell
getStudentsCount :: String -> DB.Config -> Flow Int
getStudentsCount dbName cfg = do
  (conn :: DB.Connection) <- runIO $ DB.connect dbName cfg
  students <- runIO $ DB.query conn "SELECT * FROM students"
  when (null students) $ logInfo "No records found."
  pure $ length students
```

The `runIO` method has changed since then and now the compilation will fail because `DB.Connection` does not have `ToJSON` and `FromJSON` instances. There is a simple refactoring that solves the problem in some kind: move all the DB operations into the impure block and do not expose connection out there:

```haskell
getStudentsCount :: DBName -> DB.Config -> Flow Int
getStudentsCount dbName cfg = do
  students <- runIO $ do
    (conn :: DB.Connection) <- DB.connect dbName cfg
    DB.query conn "SELECT * FROM students"
  when (null students) $ logInfo "No records found."
  pure $ length students
```

Now, the flow will be recorded as follows:

```json
{
    "entries": [
      [
        0, "RunIOEntry",
        "{\"contents\":{\"jsonResult\":\"[]\"}}"
      ],
      [
        1, "LogInfoEntry",
        "{\"contents\":{\"message\":\"No records found.\"}}"
      ]
    ]
}
```

We lost the information about DB interaction but at least the recording was successfully formed and can be replayed without errors. However, we usually want to keep the connection to the DB rather than create a new one every time we run a query. Consider the following (pretty stupid) scenario with the methods slightly reworked - this will be our primary scenario for the rest of this section:

```haskell
getStudentsCount :: String -> DB.Config -> Flow Int
getStudentsCount dbName cfg = do
  conn <- connect dbName cfg
  (students :: [Student]) <- query conn "SELECT * FROM students"
  (disabled :: [Student]) <- query conn "SELECT * FROM students WHERE disabled=1"
  let count = length students - length disabled
  when (count == 0) $ logInfo "No records found."
  pure count
```

Here, the conn value is used twice, and there is no any limitations to pass it across the scenarios. It's type is an abstraction over the native one:

```haskell
data Connection
  = NativeConn DBName DB.Connection
  | MockedConn DBName
```

It’s also serializable in sense the corresponding entry will keep some useful info about it, namely, DB name:

```haskell
data ConnectEntry = ConnectEntry
  { ceDBConfig :: DB.Config
  , ceDBName :: DBName
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

mkConnectEntry :: DBName -> DB.Config -> Connection -> ConnectEntry
mkConnectEntry dbName dbCfg _ = ConnectEntry dbCfg dbName
```

So that in the recording and normal mode the `conn` variable will contain `NativeConn`, and `MockedConn` in the replay mode. The corresponding recordings might look like this:

```json
{
    "entries": [
      [
        0, "ConnectEntry",
        "{\"contents\":{\"ceDBConfig\":\"[]\", \"ceDBName\":\"students\"}}"
      ],
      [
        1, "RunDBEntry",
        "{\"contents\":{\"dbeDescription\":\"SELECT * FROM students\",
        \"dbeJsonResult\":\"[]\",
        \"dbeDBName\":\"students\"}}"
      ],
      [
        2, "RunDBEntry",
        "{\"contents\":{\"dbeDescription\":\"SELECT * FROM students WHERE disabled=1\",
        \"dbeJsonResult\":\"[]\",
        \"dbeDBName\":\"students\"}}"
      ],
      [
        3, "LogInfoEntry",
        "{\"contents\":{\"message\":\"No records found.\"}}"
      ]
    ]
}
```

As you can see the recordings do not contain the connection itself, just a help info about it. When replaying, there should be a code in the interpreter that is able to distinguish the two variants of connection. But before we’ll have a loot at it, let’s figure out the design of the DB <-> Flow interaction that is used for the scenario above. This design utilizes a small but important idea of a clear separation between DB queries evaluation and DB connectivity management.

```haskell
type Description = String

data DatabaseF next where
  Query :: String -> ([a] -> next) -> DatabaseF next

data FlowF next where
  Connect :: DBName -> DB.Config -> (Connection -> next) -> FlowF next

  RunDB :: (ToJSON s, FromJSON s) => Connection -> Description
        -> Database s -> (s -> next) -> FlowF next
```

The Database language is auxiliary. It will be only needed to abstract the native calls, but it won’t be visible to the client code. All the actual methods will be working within the `Flow` language. The following smart constructors provide a sane UX for this DB subsystem:

```haskell
-- Helpers
query' :: String -> Database [a]
query' q = liftF $ Query q id

runDB :: (ToJSON s, FromJSON s)
      => Connection -> Description -> Database s -> Flow s
runDB conn descr db = liftF $ RunDB conn descr db id

-- Exposed DB interface
connect :: DBName -> DB.Config -> Flow Connection
connect dbName dbCfg = liftF $ Connect dbName dbCfg id

query :: (ToJSON s, FromJSON s) => Connection -> String -> Flow [s]
query conn q = runDB conn q $ query' q
```

We also provide an additional info about queries for the `RunDB` method. For example, the query string. This makes recordings more useful.

The interpreter for the `Connect` and `RunDB` methods looks similar to other methods except for `RunDB` there is a special case that checks the type of the connection, and if the latter is `NativeConn`, a real effect will be evaluated.

```haskell
interpretFlowF rt (Connect dbName dbConfig next) = do
  conn <- withRunMode (runMode rt)
    (mkConnectEntry dbName dbConfig)
    (NativeConn dbName <$> DB.connect dbName dbConfig)
  pure $ next conn

interpretFlowF rt (RunDB conn qInfo db next) = do
  res <- withRunMode (runMode rt)
    (mkRunDBEntry conn qInfo)
    (case conn of
        NativeConn _ nativeConn -> runDatabase nativeConn db
        MockedConn _            -> error "Should not be evaluated.")
  pure $ next res
```

The variant with `MockedConn` won’t be called in the replaying mode. Hopefully, no one will create a fake `MockedConn` for the normal mode.

This is how we abstract over the native DB facilities, - this “pattern” can be (and should be) used for all other native effects and subsystems. Although they can require a slightly different design, the idea will remain the same: provide a custom, possibly serializable type, do not use native types in flows, hide native calls behind a eDSL. And the flows will become clean and nice.

### Presto Backend possibilities and PureScript differences

In the [Presto.Backend](https://github.com/juspay/purescript-presto-backend/tree/feature/record-replay) framework (by [Juspay](http://juspay.in)), we’ve developed a powerful technology for automated regression testing. This particular showcase project is mostly a less-featured port from the PureScript code, so if you are interested to know more, you can check Presto.Backend. It supports such features as configs, async flows handling, KV DB and SQL DB interaction and many others. Here goes a short overview of its possibilities related to recording-replaying mechanism.

Different configs can be used for a fine tuning of the recording-replaying process.

- Recorder configs:
  * Disable entries from recording. These entries will not appear in the recording.

- Player global configs. The entries of a specified type can be configured separately:
  * Disable verifying of entries.
  * Disable mocking and verifying of entries.
  * Skip entries completely. These particular entries will be filtered out from the recordings.

Additionally, an entry can be individually configured by setting up its replaying mode. You can adjust it by editing the recording. Entry replaying modes:

- Normal (default). Mocking and verifying enabled.
- NoVerify. Verifying disabled, mocking enabled.
- NoMock. Verifying and mocking disabled. Real effect will be used on this step.

The framework also supports async evaluations, and the recording-replaying mechanism respects that. Forked flows will be recorded and replayed separately, thread-safely, without making a mess in the recording entries. This works for flows hierarchies or any size. The framework supports KV DBs and SQL DBs, and it has many other possibilities. You can find more samples of flows and recordings in tests to `Presto.Backend`, [here](https://github.com/juspay/purescript-presto-backend/blob/feature/record-replay/test/Presto/Backend/RunModesSpec.purs).

PureScript has some significant differences from Haskell on the type level. In particular, there is no `GADTs`, `Type Families` and `Existential Types` there. This is sometimes an obstacle but many cases can be solved by other tools and features. For example, we workarounded the lack of existentials by a special type [Data.Exists](https://pursuit.purescript.org/packages/purescript-exists/4.0.0/docs/Data.Exists) that we’re using to wrap our types:

```haskell
import Data.Exists (Exists)

data BackendFlowCommands next s =
  ...

newtype BackendFlowWrapper next = BackendFlowWrapper (Exists (BackendFlowCommands next))

type BackendFlow next = Free BackendFlowWrapper next
```

There is a burden in how we wrap the methods into the recordable-replayable form. Essentially, we convert a type class instance into an explicit dictionary because there is no possibility to pass type class instances via ADT methods with preserving access to the type class (no existentials and GADTs). Even more burden comes from the combo: lack of orphan instances and lack of serialization instances for some important types. Finally, effect system with row effects brings a lot of unnecessary code that is there for no practical reason. Some of these issues have been fixed in the new versions of PureScript, but we’re still on 0.11 - and have to deal with extra accidental complexity comparing to the ideal solution. Check it out for a simple method:

```haskell
data BackendFlowCommands next s
    = GenerateGUID
        (Playback.RRItemDict Playback.GenerateGUIDEntry String)
        (String -> next)

generateGUID :: String -> BackendFlow String
generateGUID description = wrap $ GenerateGUID
    (Playback.mkEntryDict description $ Playback.mkGenerateGUIDEntry description)
    id
```

But... This is fine and not a problem. We wrote this code once during the framework improvement, and we won’t touch it anymore. The business logic hasn't changed at all (except replacing the native types by their abstracted analogues), and it’s now completely recordable and replayable. Which is more important than any possible boilerplate on the implementation level.

### Conclusion

The Free monad approach allowed us to add this very useful feature almost for free. We got interesting results and revealed even more use cases than we thought initially. For instance, it’s pretty much possible to automatically measure the performance of some important steps like HTTP calls, DB interaction, and so on, and have a way to get a structured, configurable report from the recordings. This is a very cheap approach to white-box testing that has the power to save a lot of hours that are otherwise could have been lost in unit testing. The last but not the least is that having an application being structured into layers with Free monads enables a lot more possibilities that were not possible with other approaches.

### Acknowledges

Thanks to all who made this article possible:

* [Juspay Technology Private Limited](http://juspay.in/)
* Vimal Kumar, Dilip Jain, Arun Raghavan
* [Contributors to Presto.Backend](https://github.com/juspay/purescript-presto-backend/graphs/contributors)
* Vasily Gorev, Sergey Stepanenko, Shubhanshu Mani
* And other people who were working on this project.
