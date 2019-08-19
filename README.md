# Automatic White-Box Testing with Free Monads

Building applications with complex business logic is rarely done without testing. Essentially, the more complex business scenarios you have the easier it is to break something while editing the code. Pure calculations, DB interaction, communication with external services, state mutation - all these code parts may change with time, and sometimes by mistake. Moreover, external services your logic interacts with can also change even when they shouldn’t, and it will immediately make the code invalid. Thus, to be sure that the code works as it should, various testing is needed. But testing requires a lot of labour work, and it’s not really clear whether some kinds of tests are worth it. In this article, we’ll see that there is a very powerful approach to make testing as easy as possible.

**Note.** We’ve developed this approach at [Juspay](https://juspay.in/) as a feature of the framework [Presto.Backend](https://github.com/juspay/purescript-presto-backend/tree/feature/record-replay) (Open Source, Free Monad based). We’re already using it in production for our QA needs.

**Note.** This article is not an introduction into Free Monads. You can get additional information in my book ["Functional Design and Architecture"](https://github.com/graninas/Functional-Design-and-Architecture) or try another resources.

TODO
