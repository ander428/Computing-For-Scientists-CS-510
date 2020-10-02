#!/bin/bash

if [ $1 = "duck" ] || [ $1 = "Duck" ] || [ $1 = "ducks" ] || [ $1 = "Ducks" ];
then
    echo "quack"
elif [ $1 = "cow" ] || [ $1 = "Cow" ] || [ $1 = "cows" ] || [ $1 = "Cows" ];
then
    echo "moo"
elif [ $1 = "horse" ] || [ $1 = "horses" ] || [ $1 = "Horse" ] || [ $1 = "Horses" ];
then
    echo "neigh"
fi