import { describe, expect, it } from "vitest";
import * as Linguine from "../src/index";

describe("Client", () => {
  it("should create a valid client", () => {
    const config: Linguine.ClientConfig = {
      baseUrl: "http://localhost:3000",
    };
    const client = new Linguine.Client(config);

    expect(client.config).toEqual(config);

    const client2 = new Linguine.Client({
      baseUrl: "http://localhost:3000/",
    });

    expect(client2.config).toEqual(config);
  });
});
