package ltms;

import java.util.*;

public class Marx {
    public static final String[] ATTRIBUTES = {
        "PLAYS-PIANO", "PLAYS-HARP", "SMOOTH-TALKER",
        "LIKES-GAMBLING", "LIKES-ANIMALS"
    };

    public static final String[] OBJECTS = {"GROUCHO", "HARPO", "CHICO"};

    public static void marxBrothers() {
        solveAttributionProblem(ATTRIBUTES, OBJECTS);
    }

    public static List<List<Object>> makeAttributeChoiceSets(String[] attributes, String[] objects) {
        List<List<Object>> result = new ArrayList<>();
        for (String attr : attributes) {
            List<Object> choices = new ArrayList<>();
            for (String obj : objects) {
                choices.add(Arrays.asList(attr, obj));
            }
            result.add(choices);
        }
        return result;
    }

    public static void solveAttributionProblem(String[] attributes, String[] objects) {
        LTRE.inLTRE(LTRE.createLTRE("Attribution Problem Scratchpad", false));
        MarxData.loadMarxData(LTRE.currentLTRE);
        DDS.ddSearch(
            makeAttributeChoiceSets(attributes, objects),
            () -> showAttributeSolution(attributes),
            LTRE.currentLTRE
        );
    }

    public static void showAttributeSolution(String[] attributes) {
        System.out.println("\nSolution:");
        for (String attr : attributes) {
            List<Object> matches = LTRE.fetch(Arrays.asList(attr, "?object"), LTRE.currentLTRE);
            for (Object match : matches) {
                if (LTRE.isTrue(match, LTRE.currentLTRE)) {
                    System.out.printf("\n  %s", match);
                }
            }
        }
    }
}
