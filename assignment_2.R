library(tuber)
library(dplyr)

app_id= "638646750878-qscd8t98vve6qsucioa7oabpki6umu5a.apps.googleusercontent.com"
app_secret= "eTsEULZ7hFk0E03VSmHtCXHq"

yt_oauth(app_id=app_id, app_secret=app_secret, token="")

channel_videos = get_all_channel_video_stats(channel_id = "UCCJsQKOKArvDksacfT2ryQw", max_results = 1000 )
View(channel_videos)
video_url=channel_videos$url
a=strsplit(video_url, "=",2)
b=sapply( a, "[", 2 )
c=data.frame()
for(i in b){
  d=get_all_comments(video_id = i)
  c=rbind(c,d)
}
comments=c$textOriginal
write.csv(comments,"all_comments.csv", row.names = F)
disliked_index=which.max(as.integer(channel_videos$dislikeCount))
disliked_comments=get_all_comments(video_id = b[disliked_index])
disliked_comms=write.csv(disliked_comments$textOriginal,"disliked_comments.csv", row.names = F)
liked_index=which.max(as.integer(channel_videos$likeCount))
liked_comments=get_all_comments(video_id = b[liked_index])
liked_comms=write.csv(liked_comments$textOriginal,"liked_comments.csv", row.names = F)
time=channel_videos$publication_date
write.csv(time, "time.csv", row.names = F)

