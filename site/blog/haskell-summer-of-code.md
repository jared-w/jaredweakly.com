---
title: Haskell Summer of Code
date: 2017-06-13
---

Hey there! I’m Jared and I’m proud to announce that I’ll be working with the Haskell Summer of Code project; specifically, on the project "Bringing Sanity to the GHC Performance Test-suite."
It was a bit of a roller-coaster getting to this point but I’m quite excited to get cracking.

For those who want the nitty-gritty details on the project and wish to read my exact proposal, I’ve put it on my GitHub and you can view it [here](https://github.com/jared-w/HSOC2017).
For those who want a less technical introduction, here’s a bit of introduction about the project itself, the languages I’ll be working in, and what I aim to do:

## Quick Intro

First off, GHC is the compiler of the programming language Haskell.
It’s pretty neat and does lots of cool stuff; unfortunately, one of the things it isn’t super great at right now is running performance tests.
The performance tests do two things: first, they make sure that when people change the compiler, they don’t accidentally make it slower; secondly, they make sure that when people change the compiler, they don’t accidentally make the generated code less efficient.
Right now, the performance test situation requires manually generating "target numbers," which are the same for every machine and operating system–this renders them mostly useless unless you submit your patch to the build server that has a standardized testing area.

## Pain... Everywhere...

This is a giant pain in the neck when you’re just trying to hack on the compiler yourself and get something implemented.
Worse yet, because Windows, MacOS, and Linux typically all have slightly different characteristics, the performance number has to have a wide enough error tolerance to account for the operating system differences; this means that you can introduce, say, a 10% performance regression and have the tests pass because that error margin was needed to account for OS differences.
Over time, those hidden regressions can pile up if they’re not diligently caught.
So now, in order to really contribute meaningful code to the compiler, a contributor has to:

1. Constantly push code to the build server or get an intuitive feel for how much to "mentally correct" the performance numbers when running tests on their personal computer.
2. Either test constantly on all the operating systems themselves or push to the build server and suffer a massive slowdown in a feedback-loop.
3. Do tons of repeat testing to figure out whether or not the performance numbers are actually improved, constant, regressed, etc., taking into account the OS differences and the very wide tolerances on numbers set in place to account for those OS differences. A pass/failure indication of a test becomes almost meaningless; only experience tells you how to proceed.
4. Any new test or new feature that requires a test must be manually given performance numbers. The test must be run a ton of times to figure out how much error tolerance to give the number to make the test "accurate" on all of the operating systems GHC supports.
5. And so on...

That’s an impressive amount of busy-work and nonsense that a programmer has to keep up with just to not make the compiler suck.
We’re not even getting to actually improving the compiler! Clearly, this is a situation that needs to change.

## Future Bliss

The goal, at the end of this, is to have a much improved situation all around.

1. Get rid of performance metric numbers entirely from the Test-suite. Instead, they’ll be added into something called a "git note" and will be able to be added/changed per operating system and over time. The goal is to always trust a passing performance test.
2. Automate the entire process of adding performance numbers to tests, managing performance numbers, and provide tools to make things as painless as possible.
3. Allow for on-the-fly numbers generated for any particular machine. That way, developers hacking around on their own computer have a useful set of tests to run without needing to push to a remote build. The goal is to have as fast, painless, and accurate of a feedback-loop as possible.

How are we gonna achieve all of this?
I’m glad you asked!
The first goal is going to be to introduce a new test modifier to the testsuite driver (a python program that runs the tests).
This will dump collected measurements into a git note.
At this point, I won’t be messing with any of the program’s logic itself yet except for potentially starting to refactor the performance metrics out of the test-driver.
After that, a few tests will be moved over to use the new test modifier and make sure that everything’s working; things will still be manual, but when everything works, all that remains is to automate the git note population and usage–then the most basic and essential goals of the project will be done.

## Insert Hero

The plucky hero (me) and his brave companion will adventure forth to fix this and bring sanity to the GHC performance test-suite!
Will they succeed? Will they fail? Find out on this blog!
It’ll be covering the ups and downs of the journey towards sanity.
(And as a note to the technical users: I will be committing to a git branch in the GHC repo: wip/perf-testsuite. You’ll be able to see progress there, too.)
