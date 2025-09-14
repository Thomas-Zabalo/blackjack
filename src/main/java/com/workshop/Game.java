package com.workshop;

public class Game {
    private int pot;

    public Game(){
        this.pot = 0;
    }

    public int getPot(){
        return pot;
    }

    public void setPot(int pot){
        this.pot = pot;
    }

    @Override
    public String toString() {
        return "Mise de la partie [" + pot + "]";
    }
}
