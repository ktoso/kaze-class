Kaze Class
==========

Yet another "like case class but easier to evolve in binary compatible way" source code generator.

**Specific use case:** when configuration objects where prototyped during experimental phase of a project 
using case classes, and now need to be made stable API, thus generate sadly often having to resign 
from using case classes – this project takes a `Class[T]` and generates a **KazeClass** from it,
which is nothing else than a string representation of a "manual implementation" of the most basic features of case clases.

A `KazeClass` uses identity equality (not structural equality), be aware of that.

Naming
------

*Kaze* (風) means "wind" or "movement of air" in Japanese,
which reminded me of how we wanted to swiftly, and unhindered–like the wind–evolve these classes.

Usage
=====

Get the jar or copy paste the KazeClass into your project, then call:

```scala
val rendered: String = KazeClass.of[Person].render
```

to get a "desugared" case class, ready to replace your case class.

Here is an [example output](src/test/scala/pl/project13/kaze/KazeClassSpec.scala).

Goals / Non-Goals
=================

Goal: 

- One of the goals is to be able to just paste this one class (or include in REPL) to generate the source.
It must be small, trivial even, and have zero dependencies.

Non-Goal: 

- Actually become a big fancy project that solves all things regarding helping evolve the classes.
  Multiple people have attempted that, and eventually it always breaks down on some edge cases (or plain lack 
  of time), thus this code generator is dead simple and proud to be so.
- Cover all edge cases of what might go wrong, minor manual interaction is acceptable (feel free to submit PRs to minimize those though).

License
=======

Apache 2.0