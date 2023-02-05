cormatrix <- function(x){
  cormatrix <- Hmisc::rcorr(as.matrix(x))
  cormatrix.r <- cormatrix[[1]]
  cormatrix.p <- cormatrix[[3]]
  cormatrix.N <- cormatrix[[2]]
  cormatrix.rANDp<-matrix(NA,nrow=nrow(cormatrix.r),ncol=ncol(cormatrix.r))
  colnames(cormatrix.rANDp)<-colnames(cormatrix.r)
  for (i in seq_len(nrow(cormatrix.r))) {
    for (j in seq_len(ncol(cormatrix.r))){
      if (
        is.na(cormatrix.p[i,j])){
        a <- c(" ")
      }else if (round(cormatrix.p[i,j],digits = 2) <= 0.001){
        a <- c("***")
      }else if (round(cormatrix.p[i,j],digits = 2) <= 0.01 && round(cormatrix.p[i,j],digits = 2) > 0.001){
        a<-c("**")
      }else if (round(cormatrix.p[i,j],digits = 2) <= 0.05 && round(cormatrix.p[i,j],digits = 2) > 0.01){
        a<-c("*")
      }else if (round(cormatrix.p[i,j],digits = 2) > 0.05){
        a<-c(" ")
      }
      cormatrix.rANDp[i,j]<-paste0(round(cormatrix.r[i,j], digits = 2), a , collapse = "")
    }
  }
  cormatrix.rANDp <- as.data.frame(cormatrix.rANDp)
  cormatrix.rANDp
}


# Copyright 2023 Yuejia Teng, Ph.D.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
