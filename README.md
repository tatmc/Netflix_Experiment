## Netflix Experimental Analysis

This project was completed as part of the *STAT430 - Experimental Design* course taught by Nathaniel T. Stevens at the University of Waterloo (Waterloo, ON) and aims to optimize the Netflix homepage to reduce browsing time and combat decision paralysis. With so many options available, users can feel overwhelmed, which leads to longer browsing times and less engagement. To tackle this, I experimented with different factors like tile size, match score, preview length, and preview type to improve the overall experience.

- **Tile Size:** Adjusts the number of visible tiles on the screen by changing the tile’s height-to-screen-height ratio.
- **Match Score:** Predicts how much you will enjoy a show or movie based on your viewing history, expressed as a percentage.
- **Preview Length:** The duration of a show or movie's preview in seconds.
- **Preview Type:** The type of preview that is autoplayed.

The response surface simulator created by Nathaniel T. Stevens was used to simulate an experiment examining the impact of various design factors on browsing time for the Netflix homepage. The tool generated 100 browsing time observations per condition which facilitated the analysis of how factors like tile size, match score, preview length, and preview type affect user behavior.

To view the full report, click [here]().
To explore the code used in the analysis, click [here]().


### Executive Summary

The purpose of this experiment was to identify which factors significantly inﬂuence Netflix browsing time and determine which combination of these factors' levels will minimize the average browsing time. According to the Factor Screening Design, match score and preview length were found to be significantly influential. The Method of Steepest Decent was used to move from the initial region of experimentation with a center point (Preview Length, Match Score) = (110,90) towards the vicinity of the optimum and to locate optimal settings of these two factors. The contour plot for the estimated first-order response surface was produced to visualize the path of the steepest descent. According to the Method of Steepest Decent procedure, the optimal preview length was estimated to be in the vicinity of 90 sec and the optimal match score was in the vicinity of 67%. Next, a test of curvature was performed in the region with center point (Preview Length, Match Score) = (90, 67) to determine whether the vicinity of the optimum was reached. A full second-order response surface model fas fit to identify the factor levels that minimize expected browsing time. Central Composite Design was used to investigate 2 factors using 9 distinct experimental conditions. A spherical design was used to allow the experiment to be efficient since only 4 new experimental conditions were to be generated in addition to the data collected in the previous steps. A preview length of 65 sec and a match score of 80% were determined to be the optimal combination that minimizes browsing time. The estimated browsing time is 9.63325 minutes and the 95% prediction interval is (8.86378,10.40273). 
