package com.workshop;

public class Joueur {
    private int score;
    private int jeton;
    private int carte;

    public Joueur(int score, int jeton, int carte) {
        this.score = score;
        this.jeton = jeton;
        this.carte = carte;
    }

    public int getScore() {
        return score;
    }

    public void setScore(int score) {
        this.score = score;
    }

    public int getJeton() {
        return jeton;
    }
    public void setJeton(int jeton) {
        this.jeton = jeton;
    }

    public int getCarte() {
        return carte;
    }
    public void setCarte(int carte) {
        this.carte = carte;
    }

    @Override
    public String toString() {
        return "Joueur [score = " + score + ", jetons restants = " + jeton + ", cartes = " + carte + "]";
    }
}
