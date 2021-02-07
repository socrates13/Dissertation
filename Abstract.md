# Dissertation
My dissertation project: Study of KPIs which affect the outcome in Rugby Union fixtures


Game analysis is part of Sports Analytics. Sports Analytics is a contemporary field that evolves Sports Science, Computer Science, Statistics and Decision Making in order to provide teams with enhanced solutions to the main problem: Which events happened during a match affect the game outcome?
In this study, the project refers to the Scottish Rugby Union’ attempts to leverage the potentials of Sports Analytics in order to acquire a competitive advantage over their rivals and help their team stuff design new tactics and follow new strategies so that they further improve the quality of their players and consequently the game quality, and the Rugby continues to thrive and grow in Scotland.


The challenge for SRU performance analysts is to manually analyse a complex set of events happening during a match and determine which events affect the game outcome. The aim of the conjoint project between SRU and the Strathclyde Business School was the development of an effective and efficient computer-aided solution that will provide the Scottish Rugby Union performance analysts with the most updated KPIs which will bring an effect on winnings/losses. This solution should harness the potentials of Machine Learning techniques.


After retrieving data for 323 games plus a validation sample of the most successful home teams, two classifiers were constructed. After assessing these two classifiers, Classifier 2 selected. The selected classifier’s Accuracy was 75.8% in average. Twenty-two performance indicators (out of twenty-seven that the model includes) compose ten groups associated with game actions. These performance indicators were divided into two categories according to whether to reinforce or limit the odds of winning. The KPIs that increase the log(odds) of winning stemmed from actions that help teams maintain the possession, gain meters/territory and utilise individual player’s skills (power, shot/pass accuracy, velocity) in order to score tries and goal kicks mainly. On the other hand, teams that fail to defend effectively or keep possession and make errors on the transition game from their side to the opponent’s have fewer opportunities to win a match. Referee’s intervention seems positive to the game outcome for a team, but it is not clear which kind of intervention is which helps.


This classifier was considered as competitive among previous implementations. Points of development that were noticed are related to the ways of handling Not Available (NA) values and training a model and the future implementation of other Machine Learning models. Moreover, more engagement of the performance analysts could be beneficial for constructing a robust model.


Keywords: Key Performance Indicators (KPI); Rugby Union; Performance Analysis; Game Analysis; Sports Data Analysis; Sports Analytics; Scottish Rugby Union; EDA; game outcome; categorical variables; binary classification; Logistic Regression; Machine Learning Model; ANOVA; Stepwise model selection; χ^2 (chi-square) test; Z-test; R-code; PowerBI;
