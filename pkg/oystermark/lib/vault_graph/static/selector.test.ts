import { describe, expect, it } from "vitest";
import { type Selector, selectorMatches } from "./selector";

describe("selectorMatches", () => {
  it('"all" matches every label', () => {
    expect(selectorMatches("all", "anything")).toBe(true);
    expect(selectorMatches("all", "")).toBe(true);
  });

  it('"none" matches no label', () => {
    expect(selectorMatches("none", "anything")).toBe(false);
    expect(selectorMatches("none", "")).toBe(false);
  });

  describe("include selector", () => {
    const sel: Selector = { include: ["foo", "bar"] };

    it("matches labels in the include list", () => {
      expect(selectorMatches(sel, "foo")).toBe(true);
      expect(selectorMatches(sel, "bar")).toBe(true);
    });

    it("rejects labels not in the include list", () => {
      expect(selectorMatches(sel, "baz")).toBe(false);
      expect(selectorMatches(sel, "")).toBe(false);
    });
  });

  describe("exclude selector", () => {
    const sel: Selector = { exclude: ["foo", "bar"] };

    it("rejects labels in the exclude list", () => {
      expect(selectorMatches(sel, "foo")).toBe(false);
      expect(selectorMatches(sel, "bar")).toBe(false);
    });

    it("matches labels not in the exclude list", () => {
      expect(selectorMatches(sel, "baz")).toBe(true);
      expect(selectorMatches(sel, "")).toBe(true);
    });
  });

  it("include with wildcard matches only literal *", () => {
    const sel: Selector = { include: ["*"] };
    expect(selectorMatches(sel, "*")).toBe(true);
    expect(selectorMatches(sel, "foo")).toBe(false);
  });
});
