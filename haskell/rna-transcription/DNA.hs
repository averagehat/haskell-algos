module DNA where

toRNA s = map trans s
 where 
   trans 'A' = 'U'
   trans 'C' = 'G'
   trans 'G' = 'C'
   trans 'T' = 'A'
