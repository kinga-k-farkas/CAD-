df_maker<-function(df,p_df=data.frame()){
      #this recursive function takes a two column data base with only non-negative 
      #integer entries in the second column and returns something I want...
      if (nrow(df)==0) {
            print("ERROR:The input data frame is empty!")
            return(p_df)
      }
      if (nrow(df)==1 & df[1,2]==1) {
            p_df<-rbind(p_df, c(df[1,1],1))
            return(p_df)
      }
      if (nrow(df)==1 & df[1,2]==0){
            p_df<-rbind(p_df, c(df[1,1],NA))
            return(p_df)
      }
      else if (df[nrow(df),2] > 1) {
            p_df<-rbind(p_df, c(df[nrow(df),1], df[nrow(df),2]))
            new_df<-df
            new_df[nrow(new_df),2]<-new_df[nrow(new_df),2]-1
            return(df_maker(new_df, p_df))
      }
      else if (df[nrow(df),2] == 1){
            p_df<-rbind(p_df, c(df[nrow(df),1],1))
            new_df<-df
            new_df<-new_df[1:(nrow(df)-1),]
            return(df_maker(new_df, p_df))
      }
      else {
            p_df<-rbind(p_df, c(df[nrow(df),1], NA))
            new_df<-df
            new_df<-new_df[1:(nrow(df)-1),]
            return(df_maker(new_df, p_df))      
      }
}