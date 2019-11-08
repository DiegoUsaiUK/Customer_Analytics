# Steps and considerations to run a successful segmentation with K-means, Principal Components Analysis and Bootstrap Evaluation

Clustering is one of my favourite analytic methods: it resonates well with clients as I’ve found from my consulting experience, and is a relatively straightforward concept to explain non technical audiences.

Earlier this year I’ve used the popular __[K-Means clustering algorithm__](https://en.wikipedia.org/wiki/K-means_clustering) to segment customers based on their response to a series of __marketing campaigns__. For that [__post__](https://diegousai.io/2019/05/a-gentle-introduction-to-customer-segmentation/) I’ve deliberately used a basic dataset to show that it is not only a relatively easy analysis to carry out but can also help unearthing interesting patterns of behaviour in your customer base even when using few customer attributes.

In this project I revisit customer segmentation using a __complex and feature-rich dataset__ to show the practical steps you need to take and typical decisions you may face when running this type of analysis in a more realistic context.

## Business Objective

Choosing the approach to use hinges on the nature of the question/s you want to answer and the type of industry your business operates in. For this post I assume that __I’m working with a client that wants to get a better understanding of their customer base__, with particular emphasis to the __monetary value__ each customer contributes to the business’ bottom line.

One approach that lends itself well to this kind of analysis is the popular RFM segmentation, which takes into consideration 3 main attributes:

- __Recency__ – How recently did the customer purchase?
- __Frequency__ – How often do they purchase?
- __Monetary Value__ – How much do they spend?

This is a popular approach for good reasons: it’s __easy to implement__ (you just need a transactional database with client’s orders over time), and explicitly creates sub-groups based on __how much each customer is contributing__.

## The Data

In this post I’m simply loading up the compiled dataset but I’ve also written a post called [__Loading, Merging and Joining Datasets__](https://diegousai.io/2019/09/loading-merging-and-joining-datasets/) where I show how I’ve assembled the various data feeds and sorted out the likes of variable naming, new features creation and some general housekeeping tasks. You can find the full data code on this [__Github repository__](https://github.com/DiegoUsaiUK/Loading_Merging_and_Joining_Datasets).

## Links

You can find the final article on [__my website__](https://diegousai.io/2019/09/steps-and-considerations-to-run-a-successful-segmentation/)

I've also published the article on [__Towards Data Science__](https://towardsdatascience.com/practical-steps-and-considerations-to-successfully-profile-your-customer-base-19f3f0991407)
