# Perceived Experts

This repository contains scripts used to conduct the main analyses in the paper "The Role and Influence of Perceived Experts in an Anti-Vaccine Misinformation Community." Note that figure numbering may be inaccurate.

## Data

user-chars.csv characteristics of users (centrality, category, perceived expertise, and community) used in main analyses. Output of user-chars.R

user-charsn5s5.csv and user-charsn10s2.csv give characteristics of users used in the supplementary analyses comparing alternative coengagement networks. Output of user-chars-coeng-sens.R

users_formatch.csv dataframe of matching covariates used for propensity score matching (RQ4). Output of matching-covariates.R

users_formatch_n2s10 and users_formatch_n5s5 are dataframes of matching covaries for the alternative coengagement networks. Output of matching-covariates.R

link-counts-bigmods.csv dataframe with information about the types of links share by different users (where each row corresponds to a different user in one of the main communities in the main coengagement network or the two coengagement networks for the sensitivity analysis). Output of get-link-counts.R

## Scripts that can be run with provided data

Scripts under this heading are listed in the order they were run.

user-chars.R script that generates a dataframe of user characteristics used in all analyses (community and subcommunity assignment, user category, perceived expertise, and centrality). Inputs: test-n10s2.gexf and infomap-output (from network folder), user-labels-processed.csv (from user-lab folder).

user-chars-coeng-sens.R script that generates a dataframe of user characteristics used in supplemental analyses with alternative coengagement networks. Inputs: user-chars.csv, coeng-sens-user-labels.csv, infomap-output_n5s5, infomap-output_n10s2, coeng_n5s5.gexf coeng_n10s2.gexf 

results.Rmd recreates all figures and results from the main text using the files user_chars.csv, users_formatch.csv, and link-count-bigmods . Also recreate Supplemental Figures 16-20 and Tables 5-9 (main matching analyses).

results-n10s2 and results-n5s5 recreate the figures and results for the supplementary analysis comparing alternative coengagement networks. They use the corresponding user-chars and users_formatch files and link-count-bigmods.

supplement.Rmd recreates Supplemental Figures 7, 8B, 9, 21-24, and Table 4 using the files user_chars.csv, users_formatch.csv, link-count-bigmods, louvain_df.csv, and tweet_stance.csv

## Scripts that cannot be run with provided data

anonymize-users.R loops through Twitter collections and assigns a random numeric identifier to each user for anonymization purposes. Separately stores dataframes with user information (non-anonymous) and tweets (connected to anonymous identifiers). Also creates an id_dict that can map between anonymous numeric identifiers and the alternative numeric identifier assigned to users by Twitter

grab-userinfo.R loops through dataframes of users to retrieve profiles (name, username, and description) for all users in the coengagement network (for tagging). Outputs: users_to_label.csv (all users in the coengagement network) and validate-users.csv (a random sample of 500 users to assess interrater reliability), which are both stored in the user-lab folder

process-user-labels.R script that takes in raw tags for user profiles and translates them into user categories. Also calculates interrater reliability. Inputs: user-labels.csv, validate-users2.csv, and validate-users3.csv (in user-lab folder). Outputs: user-labels-processed.csv, validate-users2-processed.csv, validate-users3-processed.csv, and validate-users-all.csv (in user-lab folder)

grab-tweets.R loops through dataframes of tweets to retrieve tweets by users in the pro- and anti-vaccine communities (for the main text coengagement network and the two alternative coengagement networks used for the supplemental sensitivity analysis). Output (not included due to data sharing agreement): tweets_by_users_in_network-bigmods.csv

link-analysis.R is a script to extend shortened URLs and check whether links are low quality, academic, left-biased, or right-biased

get-link-counts.R applies the link-analysis script to assess the types of links being shared by perceived experts and perceived non-experts in the anti- and pro-vaccine communities. Output: link-counts-bigmods

get-popular-tweets.R retrieves ten most popular tweets by the ten perceived experts and perceived non-experts in the anti- and pro-vaccine communities with the greatest degree centrality. Outputs: top-tweets.csv (excluded from repository for anonymity) and paraphrased.tweets.csv (which was then further edited to remove original tweets and add paraphrases)

matching-covariates.R loops through the initial Twitter collections again to retrieve additional user covariates used in matching for perceived experts and perceived non-experts in the pro- and anti-vaccine communities. Inputs: user-chars.csv, link-counts-bigmods. Outputs: users_formatch.csv.

## Folders

### link-lists

Lists of links from academic sources and low quality sources.

iffy-ratings.csv is a list of low quality sources is from Iffy.news at <https://docs.google.com/spreadsheets/d/1ck1_FZC-97uDLIlvRJDTrGqBk0FuDe9yHkluROgpGS8/edit#gid=1144285784>

journal_links is a list of academic sources from OpenAlex <https://openalex.org/> combined with the list of pre-prints via ASAPBio <https://asapbio.org/preprint-servers>

left-bias-sources.csv and right-bias-sources.csv are sources rated as having a left or right partisan bias respectively by Media Bias/Fact Check, compiled by drusermikecrowe on Github <https://github.com/drmikecrowe/mbfcext/blob/main/docs/sources-all.json>

### network

Folder of sources pertaining to network structure and community detection.

coeng-n2s10.gexf is the graph file of the main text coengagement network, which is an output of the Docker container

coeng-n5s5.gexf and coeng-n10s2 are the graph files of the alternative coengagement network used in supplementary analysis, and are the outputs of the Docker container

users-n2s10.csv is an edgelist for the main text coengagement network, additional output of the Docker container

forinfomap is the edgelist for the coengagement network without any additional information; supplied to <https://www.mapequation.org/infomap/> for hierarchical community detection

infomap-output gives the results of hierarchical community detection from <https://www.mapequation.org/infomap/>

infomap-output_n5s5 and infomap-output_n10s2 give the results of hierarchical community detection for the alternative coengagement networks used in supplementary analyses 

louvain_df.csv gives the communities detected using the Louvain algorithm (implemented in Gephi)

submod_prop is Table 4 (formatted for Latex) that examines subcommunities and compares community detection using Louvain versus Infomap

### user-lab

coeng-sens-user-labels.csv is the list of labeled user profiles for users not included in the main text analysis who were included in the alternative coengagement networks for the sensitivity analysis

users_to_label.csv is the list of user profiles in the coengagement network (output of grab-user-info.R)

validate-users.csv is a list of 500 random user profiles in the coengagement network used to test interrater reliability (output of grab-user-info.R)

user-labels.csv is the list of all user profiles coded by a single author

validate-users2.csv and validate-users3.csv is a list of 500 user profiles used in validation, each with codes from a different author

Files with the suffix "-processed" are profiles with category labels. The beginning part of the file name indicates the name of the file that was processed using the process-user-labels.csv

validate-users-all.csv gives the codes assigned to each user by all three authors who coded user profiles to allow for assessment of interrater reliability

### psm

Contains tables (formatted for latex) that were outputs of propensity score matching.

Prefixes indicate the hypothesis tested using matching (h1: influence boost for perceived experts in anti-vaccine community; h2: influence boost for perceived experts in pro-vaccine community; h3: greater influence boost for perceived experts in anti-vaccine community compared to pro-vaccine community).

For each hypothesis, we give:

sample-size: the number of perceived experts and perceived non-experts before and after matching

unmatched-summary: statistics about matching covariates prior to matching

matched-summary: statistics about matching covariates after matching

att: the average treatment effect on the treated for each influence metric. For H3, we provide both the calculate ATT within each community (att-within) and the difference in ATT between the two community (att-diff).

### tweet-lab

Contains files related to tagging tweet stance to describe communities.

paraphrased-tweets.csv provides examples of how tweet stance was assessed. This file is based on a sample of forty of tagged tweets (ten each, randomly sampled from combination of perceived experts or perceived non-experts in the anti- or pro-vaccine communities. Tweets are paraphrased to preserve user anonymity. Stance assessments by all three coders is provided for each tweets.

tweet_stance.csv is the file used to generate Supplementary Figure 7, which provides the stance assessment by all three coders for popular tweets from central users.

## Additional information

Docker script to generate coengagement projection (using <https://github.com/uwcip-research/Coengagement-Networks>)

docker run --rm -v C:/Users/mallj/ddd/dat:/input_data -v C:/Users/mallj/ddd/dat/forgephi:/output_data -it ghcr.io/uwcip-research/shared-engagement-projection:latest --input-data /input_data/directed-graph.csv --output-graph /output_data/test.gexf -n 10 -s 2
