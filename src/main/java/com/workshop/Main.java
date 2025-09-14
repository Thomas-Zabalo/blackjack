package com.workshop;

import java.util.*;

public class Main {
    public static void main(String[] args) {

        Joueur joueur = new Joueur(0, 200, 0);
        Croupier croupier = new Croupier(0, 0);
        Game game = new Game();
        Scanner sc = new Scanner(System.in);

        String[] valeurs = {"As", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Valet", "Dame", "Roi"};
        String[] couleurs = {"Pique", "Trèfle", "Carreau", "Cœur"};

        List<Carte> cartes = new ArrayList<>();

        for (int i = 0; i < 6; i++) {
            for (String valeur : valeurs) {
                for (String couleur : couleurs) {
                    cartes.add(new Carte(valeur, couleur));
                }
            }
        }

        Collections.shuffle(cartes);

        System.out.println("Welcome to the BlackJack game");
        System.out.println("------------------------------\n");
        if (joueur.getJeton() == 0) {
            System.out.println("Le joueur n'a pas assez de jeton pour jouer");
        }
        else {

            int index = 0; // pour avancer dans le paquet
            boolean play = true;
            while (play) {
                System.out.println("\n1: Voulez-vous jouer ?");
                System.out.println("2: Quitter le jeu !");
                int menuChoice = sc.nextInt();

                switch (menuChoice) {
                    case 1:
                        System.out.println(joueur);
                        System.out.println(croupier);

                        System.out.println("Quel est votre mise\n");

                        int mise = 0;
                        mise = sc.nextInt();
                        while (mise > joueur.getJeton()) {
                            System.out.println("Mise trop haute\n");
                            System.out.println("Quel est votre mise");
                            mise = sc.nextInt();
                        }

                        game.setPot(mise);
                        joueur.setJeton(joueur.getJeton() - mise);

                        System.out.println("Distribution des cartes\n");

                        Carte joueurCard1 = cartes.get(index++);
                        Carte croupierCard1 = cartes.get(index++);
                        Carte joueurCard2 = cartes.get(index++);
                        Carte croupierCard2 = cartes.get(index++);

                        joueur.setScore(joueurCard1.getValeurNumerique() + joueurCard2.getValeurNumerique());
                        croupier.setScore(croupierCard1.getValeurNumerique());

                        System.out.println("Vos cartes : " + joueurCard1 + " et " + joueurCard2);
                        System.out.println("Score joueur : " + joueur.getScore());
                        System.out.println("Carte visible du croupier : " + croupierCard1);

                        // Choix du joueur
                        boolean continuer = true;
                        while (continuer) {
                            System.out.println("\nQue voulez-vous faire ?");
                            System.out.println("1: Tirer une autre carte");
                            System.out.println("2: Rester");

                            int choice = sc.nextInt();
                            switch (choice) {
                                case 1:
                                    Carte newCard = cartes.get(index++);
                                    joueur.setScore(joueur.getScore() + newCard.getValeurNumerique());
                                    System.out.println("Vous tirez : " + newCard);
                                    System.out.println("Nouveau score joueur : " + joueur.getScore());

                                    if (joueur.getScore() > 21) {
                                        System.out.println("💀 Vous perdez d'office !");
                                        joueur.setScore(0);
                                        croupier.setScore(0);
                                        continuer = false;
                                    }
                                    break;

                                case 2:
                                    continuer = false;
                                    croupier.setScore(croupier.getScore() + croupierCard2.getValeurNumerique());
                                    System.out.println("Le croupier révèle sa carte cachée : " + croupierCard2);
                                    System.out.println("Score croupier : " + croupier.getScore());
                                    break;
                            }
                        }

                        // Tour du croupier si le joueur n’a pas bust
                        if (joueur.getScore() > 0) {
                            while (croupier.getScore() < 17) {
                                Carte card = cartes.get(index++);
                                croupier.setScore(croupier.getScore() + card.getValeurNumerique());
                                System.out.println("Le croupier tire : " + card);
                                System.out.println("Score croupier : " + croupier.getScore());
                            }

                            if (croupier.getScore() > 21 || joueur.getScore() > croupier.getScore()) {
                                System.out.println("🎉 Vous gagnez !");
                                joueur.setJeton(joueur.getJeton() + mise * 2);
                            } else if (joueur.getScore() < croupier.getScore()) {
                                System.out.println("Le croupier gagne !");
                            } else {
                                System.out.println("Égalité !");
                                joueur.setJeton(joueur.getJeton() + mise); // on lui rend sa mise
                            }
                        }

                        joueur.setScore(0);
                        croupier.setScore(0);
                        break;

                    case 2:
                        play = false;
                        break;
                }
            }
        }

        sc.close();
    }
}
