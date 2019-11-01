---
title: Beginner Mistakes and Oddities I Encountered
date: 2017-08-29
---

Time sure does fly don’t it?
I’m going to go over a lot of the beginner mistakes and little pitfalls and papercuts I’ve encountered so far while working on the GHC test-suite.
In a later post that I’m going to write pretty soon, I’ll be going over my progress so far, the remaining things I have yet to finish for the conclusion of the HSOC, and an epilogue of where to go from there.
Be sure to checkout the [Reddit discussion](https://www.reddit.com/r/haskell/comments/6wt11z/beginner_mistakes_and_oddities_i_encountered/) if you haven’t already.

## Project specific issues

I had a lot of these; mostly due to my inexperience, but also GHC is a pretty old project and has a lot of small warts and niggles here and there that people just end up "learning" over time.
Unfortunately it’s not really something where comprehensive documentation can easily be written down other than a general "best practices" approach (which I think I’ll write up).

* GHC has a lot of submodules. Before working you should not only pull the master but you should also run `git submodule update`; I had quite a few issues early on with extraneous files in git status before I learned to do that.
* Git notes is really not that ergonomic at all. If you have any namespaces used in your git notes (which this project does) you’ll need –ref=perf for damn near every command you use. It’s a small thing, but it can sometimes take a while to realize what you missed.
* Running the ./validate script is the only way to guarantee a clean anything. Switch a branch and something weird breaks? ./validate; Not quite sure why tests started failing after a git pull and a small innocuous code changes? ./validate. The real bummer here is that ./validate wipes everything and rebuilds the entire damn compiler and all that stuff from scratch and then runs the most extensive version of the testsuite. The exhaustive version of the testsuite alone can take nearly 2 hours on my computer if it’s not run threaded, so it’s definitely a huge time sink if you don’t have this thing optimized.
    * Related pain-point; there’s a lot of things you can do to tweak and fix your setup so that you can build and validate quicker. Unfortunately, this is all something that you just have to kind of learn over time and there’s no real way to intelligently "auto-configure" this nicely. You’ll have to suffer super long build times for GHC and long runtimes for the testsuite or sink quite a few hours into configuring the "quicker options" for your specific usecase.
    * Update: Thanks to a few helpful Reddit comments, I’m now aware that `dist/maintainer clean` instead of ./validate will help you out quite a bit. Unfortunately, this still doesn’t clean out everything.
    * To thoroughly clean out the ghc repo, you’ll need to copy mk/build.mk (and everything else you don’t want to lose) somewhere else and then run git clean -ffdx && git submodule foreach git clean -ffdx.
    * Alternatively, a gentle clean can be done by deleting `compiler/stage2` without messing up a stage 1 or library build, which can save a lot of time depending on your use cases.
* GHC uses Arcanist and Phabricator for its code diffs. I’ve never seen any other project use these things, so that’s not super helpful for knowing how to use them. With arcanist (the CLI tool for phabricator), you really want to explicitly name every single commit you push to phabricator; otherwise it guesses and it’s terrible at guessing.
* Ironically enough, the testsuite has no tests. There’s no way to know whether or not you broke something in the testsuite without running it on every single test and making sure nothing broke that wasn’t supposed to (of course, this doesn’t catch false positives...). ./validate helps, but it’s painful.
* The testsuite was pretty confusing to understand at first. There was no high level documentation really detailing how the codebase works from the perspective of on-boarding someone to work on it. While there’s enough documentation on how to _use_ it, there’s very little in the way of why it’s designed the way it is and how things fit together. A lot of my earlier weeks were wasted just screwing around and reading the code and being super unproductive in general due to that.

## Git related mistakes

Yeah, there were so many of these that it deserves its own category.
It turns out that for basic usage all you need to know is "push, pull, commit, add" but for working on a real codebase with real code and tons of things you can’t just nuke every time you do something wrong...
It’s a bit trickier.
Things I really needed and had to learn:

* Really basic, but it’s not really something I see mentioned a lot. With git, it’s totally okay to not add every file when you commit things; I committed some files I didn’t need to before I learned that.
* cherry picking (and the fact that you really want to cherry pick with only one commit, so there was some squashing involved using temporary branches)
* Merging. Every time you think you know how to merge things cleanly enough, you’re wrong.
* Related to that is rebasing. Ohhh, man, I have a section of my git log where you literally see the same commits 3-4 times because of how screwy and non-linear my history got before I cleaned it up some. Luckily for me I got much better at git hygiene after that and my issues were much less.
* Messing around with branches was another one. Sometimes you’ll have something you’re working on and then you need to start working on something else. You /could/ just put that thing in the same branch, or you could do the SMART thing and make a new branch. But oh no, what if you have tons of ADHD and you work on both branches at the same time and need to merge them together again? What do you do? You weep silently, my child, as to not disturb your sleeping girlfriend.

## Sanity checks

Programmers need sanity checks for everything, especially when working in a language like python.
I can’t begin to mention how many times I was working on something only to have everything break for hours before I realized I was implicitly assuming something that wasn’t accurate.
Not all tests are performance tests, some tests involve measuring the compiler, some don’t, etc.
Sanity checks don’t just involve the code, they also involve your environment as well;
I’ve accidentally ran tests on the wrong commit, used the wrong flags, accidentally wasted 2 hours building the wrong version of a program...
Small things like that sneak up on you and the only way to really solve them is to approach things in a certain way.

* Be deliberate: Make sure that you’re doing what you want to do with the right version of what you want to do it with.
* Hold nothing in your head. Be explicit about every assumption.

Are you assuming that everything a performance test?
Write that in a comment somewhere.
Do you have a variety of scenarios where things need to hold?
Write those down and test them every time.
(example: I lost quite a few hours once when I forgot that the performance comparison tool couldn’t assume multiple commits of information. It had to work if you had no git notes setup yet, if you only had one commit to compare with, if you had multiple, etc).
Does some condition need to hold? Check that condition rather than some other condition which implies the one you actually want.
(example: I had some code break because the testsuite was originally built in a way that assumed if one particular thing existed then the program was in a certain state. What should’ve happened is the program should’ve just checked for that state explicitly.)

On a related note, it’s really important for me to write down some notes while I’m debugging things so I can keep my train of thought if I want to take a break and come back.
There were quite a few times where I would go through something and figure out that X was doing Y and then I’d take a break and I’d go back and I’d be debugging some other area and I’d rediscover that same X doing Y again.
Keeping some notes together whenever trying to piece something together lets you hold a lot more things together and see how they’re all related.
This is even more important in a language like Python where lots of small moving parts are involved and they can all break each other.

## Error Handling

Error handling should be robust, but... Not overly excessive.
I had one issue that came up where my git notes parsing function broke if there were empty lines in the git notes.
If I had been more robust with error checking code elsewhere, I likely wouldn’t have noticed that until much later and it would’ve been harder to track down.
Thinking of functions as semi-pure helps a lot with figuring out where to put the error handling; I like to write my functions in a way that it always returns a "sane answer" to whomever called it.
Error handling should be bottled up inside as tightly as possible and only blown out if it absolutely needs to be.
In this case, the solution was to strip empty lines and then the function worked fine; if I had blown up the error elsewhere, I would’ve needed to explicitly handle it everywhere which would be a giant pain.
Instead, the git notes parsing function is a "pure" function that returns either an empty list or the parsed information and life is grand.

## Do only one thing at a time

This one is really difficult for me because I’m fairly ADHD and tend to bounce around a lot everywhere.
If I see a small little issue, I’ll fix it; if I see a small little thing, I’ll poke it; before you know it, there’s 15 unrelated changes in a commit and it looks terrible.
Instead, what I’ve learned to do is keep a notepad (or org document) handy and I just write down all the small shit I wanna fix.
Then, whenever I’m working on code, I’ll design the commit before I actually start working on it: that is, I figure out what atomic change or improvement I’m going to make and then go do that.
I’m not super strict with following this methodology yet but it’s working fantastically so far.

Another thing I have trouble with is that often, "one thing" to me is really more like 20-30 different things;
you want to work as small as possible and commit as often as possible and then squish commits down later if needed.
For example, "implement git notes parsing" is one item but it really should be made up of ~20 commits that are later squashed into one.
I’m not quite there with the squashing and committing yet, but I’m getting better.

## Taking care of yourself

This one is somewhat obvious but it’s still something that bears repeating.
I tend to get hyperfocus and I’ll either not work on a project or I’ll sink 35 hours in 2 days on a project.
That’s not particularly healthy nor is it particularly effective; it’s fine when you’re just dinking around on things, but when efficiency and critical thinking matters, it’s no longer very sufficient.
Eat regularly, sleep regularly, take breaks regularly, and work regularly; the mind thrives on consistency and learns to work well when it’s given that consistency.
If I didn’t do these things, I’d find that I would have a constant feeling of "needing to work on the project" but what I would usually end up doing was just screwing around on the computer rather than getting things done in a productive manner.
Closely related to all of this is optimizing your workflow for you and being able to recognize your limitations and how to get around them.
One thing that’s critical for me is being able to immediately answer the question "what is the next immediate task I need to do"; if I can’t answer that, I need to sit down and figure out that answer and get a small checklist written before I start working, otherwise I’ll just wander around aimlessly for a while until I sorta get stuff done.
This applies to everything in my life and it’s why making breakfast takes me an hour some days and 3 minutes other days and why I can get ready for the day in 30 minutes or 3 hours.

## Conclusion

So these are just some of the things I’ve learned and struggled with while working on this project. I’d probably summarize this into a few key points:

* Abstraction is the process of communicating more precisely about something. Use it whenever possible and helpful to make code more robust.
* Be explicit about your assumptions and use implicit behavior as little as possible; ideally document that implicit behavior in a comment whenever you can.
* Be deliberate and methodical about your sanity checks; write them down so you can verify things consistently and thoroughly.
* Do _one_ thing at a time, learn your git hygiene, and use it.
* Write down your thought processes somewhere whenever you’re doing something more intensive than fixing an immediate, small issue.
* Take care of yourself, set hours, take breaks, etc.

As always, feel free to comment on the [Reddit](https://www.reddit.com/r/haskell/comments/6wt11z/beginner_mistakes_and_oddities_i_encountered/) discussion in r/haskell.
