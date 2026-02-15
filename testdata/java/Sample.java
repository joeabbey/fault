package com.example.sample;

import java.util.List;
import java.util.ArrayList;
import java.util.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.*;

/**
 * A sample Java class for testing the parser.
 */
public class Sample extends BaseClass implements Serializable {

    private String name;
    public static final int MAX_SIZE = 100;
    protected List<String> items;

    public Sample(String name) {
        this.name = name;
        this.items = new ArrayList<>();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    private int calculate(int a, int b) {
        return a + b;
    }

    public static List<String> getDefaultItems() {
        return new ArrayList<>();
    }

    @Override
    public String toString() {
        return "Sample{name=" + name + "}";
    }
}

interface Configurable {
    void configure(String key, String value);
}

public enum Color {
    RED, GREEN, BLUE;
}

public record Point(int x, int y) {
}
