package com.workshop;

public class Carte {
    private String valeur;
    private String couleur;

    public Carte(String valeur, String couleur) {
        this.valeur = valeur;
        this.couleur = couleur;
    }

    public String getValeur() {
        return valeur;
    }

    public String getCouleur() {
        return couleur;
    }

    public int getValeurNumerique() {
        return switch (valeur) {
            case "As" -> 11;
            case "Valet", "Dame", "Roi", "10" -> 10;
            default -> Integer.parseInt(valeur);
        };
    }

    @Override
    public String toString() {
        return valeur + " de " + couleur;
    }
}
