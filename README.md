# LWSurvey
Data science tutorial based on LW Survey

##Setup

If you're new to using R, follow the instructions in "setup.md."

##The data set

The dataset in cleanedNotFilled.csv consists of a subset of the data from the [2014 Less Wrong survey](http://lesswrong.com/lw/lhg/2014_survey_results/). The code that I used to clean it is in "LWSurveyCleaner.R". The idea here is to give a simplified version of the dataset that's easy to explore. I:

1.  Restricted to respondents who answered the question about whether they had been diagnosed with depression
2.  Removed the calibration question and answer features, and a few others.
3.  Removed answer choices that few respondents offered.
4.  Renamed some of the features and answer choices so as to make them shorter and more human readable. 
5.  Took logartihms of features corresponding to Karma Score, Income, and charitable donations
