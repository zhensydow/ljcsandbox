\begin{code}
check (c,d,e,f,g,h,i) = 362880*2 + 40320*6 + 5040*c + 720*d + 120*e + 24*f + 6*g + 2*h + i
\end{code}

\begin{code}
test = [  (c,d,e,f,g,h,i) | c<-[0..7],d<-[0..6],e<-[0..5],f<-[0..4],g<-[0..3],h<-[0..2],i<-[0..1], check (c,d,e,f,g,h,i) == 1000000]
\end{code}