# scala-fusion
[![CircleCI](https://circleci.com/gh/nathaniel-may/scala-fusion.svg?style=svg)](https://circleci.com/gh/nathaniel-may/scala-fusion)
[![codecov](https://codecov.io/gh/nathaniel-may/scala-fusion/branch/master/graph/badge.svg)](https://codecov.io/gh/nathaniel-may/scala-fusion)

GHC-inspired stream fusion for scala

This repository implements techniques found in "[Stream Fusion. From Lists to Streams to Nothing at All](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.104.7401)", by Duncan Coutts, Roman Leshchinskiy, and Don Stewart (ICFP '07).

## Usage
```scala
import fusion.syntax._

LazyList(0,1,2,3,4)
  .fuse
  .map { _ + 1 }
  .filter { _ <= 2 }
  .take(1)
  .toList

// output:  List(1)
// runtime: LazyList(<not computed>) -> Stream(<instruction set>, LazyList(<not computed>)) -> List(1)
```

## Comparison
Although the usage is similar to `SeqView` in Scala 2.13+ and the resulting optimizations are similar, the implementation itself is very different. SeqView is implemented imperatively, while scala-fusion is written functionally but generates an efficient imperative instruction set at compile time.

```scala
import fusion.syntax._

// no fusion
List(0,1,2,3,4)
  .map { _ + 1 }
  .filter { _ <= 2 }
  .take(1)

// output:  List(2)
// runtime: List(0,1,2,3,4) -> List(1,2,3,4,5) -> List(1,2) -> List(1)

// SeqView
List(0,1,2,3,4)
  .view
  .map { _ + 1 }
  .filter { _ <= 2 }
  .take(1)
  .toList

// output:  List(1)
// runtime: List(0,1,2,3,4) -> SeqOps(<instruction set>) -> List(1)

// scala-fusion (this library)
LazyList(0,1,2,3,4)
  .fuse
  .map { _ + 1 }
  .filter { _ <= 2 }
  .take(1)
  .toList

// output:  List(1)
// runtime: LazyList(<not computed>) -> Stream(<instruction set>, LazyList(<not computed>)) -> List(1)
```

## Implementation
The original version relies on GHC rewrite rules to reduce `stream compose unstream` to the identity on streams. Since this is unavailable in scalac, The `Fuser` type was introduced which wraps the underlying stream and contains an instance of a type-threaded list which stores the sequence of procedures. Composing this type-threaded list results in a single set of fused instructions to run on the original input Stream. This implementation adds a very small amount of overhead proportional to the number of operations called on the stream.