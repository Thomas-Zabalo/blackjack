package com.workshop;

public class Croupier {
    private int score;
    private int carte;

    public Croupier(int score, int carte){
        this.score = score;
        this.carte = carte;
    }

    public int getScore(){
        return score;
    }

    public void setScore(int score){
        this.score = score;
    }

    public int getCarte(){
        return carte;
    }
    public void setCarte(int carte){
        this.carte = carte;
    }

    @Override
    public String toString() {
        return "Croupier [score = " + score + " carte = " + carte + "]";
    }
}
