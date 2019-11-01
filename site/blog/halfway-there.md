---
title: Halfway There, or "I want to say I know what I’m doing, but it’s a dangerous thing to say"
date: 2017-07-20
---

Greetings and salutations! If you’re reading this, it means I haven’t died yet, which is pretty great.
So, as my last blog post mentioned, I’ve been spending this summer working on a Haskell Summer of Code project: [Bringing Sanity to the GHC Performance Test-Suite](/posts/haskell-summer-of-code).
My last blog post also mentioned that I would hopefully be having frequent blog updates, but let’s not talk about that right now.
Far more exciting, here, is the potential for me to be able to write more frequently now that I have a proper handle on things!

It turns out that it’s exceedingly hard to write interesting content when you spend the majority of your time in front of a keyboard going "wat" and "I hope this doesn’t blow something up."
However, once you start to get past that stage, stare into the abyss of insanity and not go insane, yadda yadda, life becomes more fun.
In other words, I spent the first few weeks of the project being depressingly unproductive before, suddenly, a beam of light descended from the heavens and kissed me with the gift of "sorta-kinda understand stuff."
After that, things have been going increasingly swimmingly and I’m sure my mentor, Ben, appreciates my more coherent questions.

## Without further ado, the current state of the union

* There now exists functionality in the code to grab all of the performance metrics from the performance tests and collect them in a python list, where each element is a string of the form: (test_env , name , way , field , val)
* There now exists infrastructure in the code to support running only performance tests. It’s a little nicety that helped me understand the codebase better and should hopefully prove to be occasionally useful to people who need it.
* Infrastructure is now setup so that, from the commandline, one can use the new options: TEST_ENV="my environment" , USE_GIT_NOTES=YES, ONLY_PERF_TESTS=YES.
    * Currently using git notes is opt in (you must use the command line option); eventually it will be default and perhaps not even able to be opted out of. At the conclusion of the whole project, it would be great if we didn’t even need any all.T files at all.
    * TEST_ENV currently defaults to the string "local" if not set by the user. The CI builders will build with a TEST_ENV="my special environment" flag.
* After collecting the information, if USE_GIT_NOTES is set, it’ll automatically be loaded into git notes for that commit.
* Similarly, data from git notes can now be loaded into the test driver.
* Finally, I’ve started working on some tooling to start comparing performance metrics across commits.

This progress corresponds to, in my proposal, basically completing phase one. What remains now is still quite a bit, but the first big hurdle is done. The comparisons done right now in the test driver have basically not been modified much in any way; it was right around the point where I finished adding stuff into git notes that I realized we’re going to need a separate tool for sane performance metric comparison; so, I’ve done some preliminary work on that. Unfortunately, git notes are a bit tricky and not very ergonomic to work with so I’ll be working on making that as seamless as possible as well; ideally, nobody will ever need to touch them or even know they exist. Technically, at this point, the ticket for this project could probably be closed as
"successfully completed", but it’d be a shame to do so without developing the tooling to help make using the performance test-suite as painless as possible.

## What now?

At first, I was thinking that the next logical step towards working on the tooling and the project was going to be working on things like: Developing tooling to make sure that the right information is added to git notes, that the correct information is maintained and propagated into the future, into different branches, etc.,; and the auto builders will need some additional configuration in order to handle the git notes (git notes do not work very seamlessly). But then, with Ben’s help, I fleshed out the workflows a bit and realized that none of that really needs to happen if I just rip all of the performance metric comparison out of the test-driver. After that, things fell into place a bit more and the new plan is:

* Create a "library" to import into the test-driver. It’ll contain a few tools:
    * Boolean "pass/fail" function to compare whether a test varies beyond a certain variance % between two commits (generally HEAD and HEAD~1).
    * Helper utilities to load up git notes, store git notes, etc.
    * Other things as I think of them or the need presents itself.
* Flesh out the current proof of concept comparison utility:
    * The table output is currently useless for actually ‘comparing.’ I need to fix that.
    * While I’m at it, I need to tag each bit of data I grab with the commit it came from so that I can organize the data from oldest commit to newest commit and then group it by test.
    * Nifty features here and there as I figure out new ways to make people’s lives easier.

If you have any questions, feel free to ask them on reddit where this will be cross-posted, or on #haskell (my nick is jared-w), or feel free to stalk my facebook and send super creepy messages at 3am (maybe don’t do that).
