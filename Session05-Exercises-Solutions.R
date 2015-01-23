load("vote-socdem-anes48.RData")
voted.socdem.anes48 <- subset(vote.socdem.anes48,
                              vote != "DID NOT VOTE")

voted.socdem.anes48 <- within(voted.socdem.anes48,{
  voteDewey  <- vote == "VOTED - FOR DEWEY"
  voteTruman <- vote == "VOTED - FOR TRUMAN"
  voteOther  <- !(vote %in% c("VOTED - FOR DEWEY",
                              "VOTED - FOR TRUMAN")) 
})

vote.occ.long <- reshape(voted.socdem.anes48,
                         varying=list(c("voteDewey","voteTruman","voteOther")),
                         v.names="vote",
                         timevar="Candidate",
                         times=c("Dewey","Truman","Other"),
                         drop=c("vote","unionized.hh","gender","race","age",
                                "education","total.income","religious.pref"),
                         direction="long"
)
vote.occ.long <- vote.occ.long[with(vote.occ.long,
                                    order(id,Candidate)),]
vote.occ.long[1:10,]

lijphart <- read.csv("lijphart.csv")

library(foreign)
write.dta(lijphart,file="lijphart.dta")

lijphart <- read.dta("lijphart.dta")