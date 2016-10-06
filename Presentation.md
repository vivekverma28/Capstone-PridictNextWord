Word Psychic
========================================================
author: Vivek Verma
date: October 2016

A word prediction app ([link](https://vivekverma28.shinyapps.io/my_app/))

Made for Coursera data science specialization, Swiftkey capstone project.

App description
========================================================

The app lets the user type in a phrase. It predicts
the most likely next word, based on frequently
occurring phrases (n-grams).

__How it was made:__

I used 2-grams to 5-grams,
drawn from ~12,000 lines of English from blogs, news articles and Twitter.

I cleaned the text to remove things like punctuation (except apostrophes).


App function and instructions
========================================================
__How the app works:__

1. User inputs a phrase 
2. App looks at the last 1-4 words in phrase
3. App checks the database for phrases that match
4. App gathers all the possible predicted words and displays the most likely one

__How to use the app:__

* Instant mode: Type into the text field, and wait. The app
automatically displays the predicted next word.

Behind the scenes: Algorithm used
========================================================
The algorithm used is _Stupid Backoff_, described in Brants et al, 2007 ([link, see section 4](http://www.aclweb.org/anthology/D07-1090.pdf)).

I chose this because it is inexpensive and loads fast, while
performing nearly as well as Kneser-Ney smoothing -- which is quite good.

Algorithm walk-through example
========================================================
User input: `"The CAT, in the ?"`. What the algo does is:

- Cleans up and standardizes the input, turning it into: `"the cat in the"`.

- Check 5-gram data for all occurrences of `the cat in the *`, where * denotes
any word. Similarly, check 4-gram data for `cat in the *`, check 3-gram data for `in the *`, check 2-gram data for `the *`. Make a list of all the `*` candidate words.

- For each word, find maximum likelihood estimate (MLE) in the corresponding n-gram and produce a score table. 
