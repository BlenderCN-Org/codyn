#!/bin/bash

file=cdn-listings.tex

echo '\lstdefinelanguage{cdn}{%
  morekeywords={%' > $file

keywords=$(./list-emacs-keywords.sh)

n=0
for i in $keywords; do
    if [ $n -ne 0 ]; then
        echo -n ',' >> $file
    fi

    if [[ $((n%10)) == 0 ]]; then
        if [[ $n != 0 ]]; then
            echo -ne '%\n' >> $file
        fi

        echo -ne '\t' >> $file
    fi

    echo -n "$i" >> $file
    
    let n++
done

echo '},%
   morecomment=[l]\#,%
   morestring=[b]"%
  }[keywords,comments,strings]' >> $file
