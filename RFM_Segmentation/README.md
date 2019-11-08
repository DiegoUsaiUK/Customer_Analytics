# Steps and considerations to run a successful segmentation with K-means, Principal Components Analysis and Bootstrap Evaluation

In this project I use a __complex and feature-rich dataset__  to run through the practical steps you need to take and considerations you may face when running a customer profiling analysis. I  use the __K-means clustering technique__ on a range of different customer attributes __to look for potential sub-groups__ in the customer base, __visually examine the clusters with Principal Components Analysis__, and __validate the cluster’s stability with clusterboot__ from the fpc package.

I assume that __I’m working with a client that wants to get a better understanding of their customer base__, with particular emphasis to the __monetary value__ each customer contributes to the business’ bottom line.

One approach that lends itself well to this kind of analysis is the popular __RFM segmentation__, which takes into consideration 3 main attributes:

- __Recency__ – How recently did the customer purchase?
- __Frequency__ – How often do they purchase?
- __Monetary Value__ – How much do they spend?

This is a popular approach for good reasons: it’s __easy to implement__ (you just need a transactional database with client’s orders over time), and explicitly creates sub-groups based on __how much each customer is contributing__.

This analysis should provide a solid base for discussion with relevant business stakeholders. Normally I would present my client with a variety of customer profiles based on different combinations of customer features and formulate my own data-driven recommendations. However, it is ultimately down to them to decide how many groups they want settle for and what characteristics each segment should have.

## The Data

In this post I’m simply loading up the compiled dataset but I’ve also written a post called [__Loading, Merging and Joining Datasets__](https://diegousai.io/2019/09/loading-merging-and-joining-datasets/) where I show how I’ve assembled the various data feeds and sorted out the likes of variable naming, new features creation and some general housekeeping tasks. You can find the full data code on this [__Github repository__](https://github.com/DiegoUsaiUK/Loading_Merging_and_Joining_Datasets).

## Links

You can find the final article on [__my website__](https://diegousai.io/2019/09/steps-and-considerations-to-run-a-successful-segmentation/)

I've also published the article on [__Towards Data Science__](https://towardsdatascience.com/practical-steps-and-considerations-to-successfully-profile-your-customer-base-19f3f0991407)
