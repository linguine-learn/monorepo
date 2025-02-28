import { describe, expect, it } from "vitest";
import makeFetchCookie from 'fetch-cookie';

import * as Linguine from "../src/index";

describe("Auth", () => {
  const fetcher = makeFetchCookie(fetch);
  const client = new Linguine.Client({
    baseUrl: "http://localhost:3000/",
    fetcher
  })

  it("should create an auth module", () => {
    const store =  new Map<string, string>();

    const storageHandler = {
      set: (key: string, value: string) => {
        store.set(key, value);
      },
      get: (key: string) => {
        return store.get(key)
      },
      delete: (key: string) => {
        store.delete(key);
      }
    } as Linguine.TokenStore;

    const config: Linguine.AuthClientConfig = {
      accessTokenStorage: storageHandler,
    };
    const authClient = new Linguine.AuthClient(client, config);

    expect(authClient.config).toEqual(config);
    expect(authClient.client).toBe(client);
  })

  it("should create and login a user", async () => {
    const accessTokenStorage = new Map();

    const authClient = new Linguine.AuthClient(client, {
      accessTokenStorage,
    });

    const registerResponse = await authClient.register({
      email: "test@example.com",
      username: "myUniqueUsername",
      password: "password",
      retypedPassword: "password",
    });

    expect(registerResponse).toBe(true);

    const loginResponse = await authClient.logIn({
      email: "test@example.com",
      password: "password",
    });

    expect(loginResponse).toBe(true)

    expect(accessTokenStorage.size).toBe(1);
    expect(accessTokenStorage.has("accessToken")).toBeTruthy();
  })

  it("should properly refresh the tokens", async () => {
    const accessTokenStorage = new Map();

    const authClient = new Linguine.AuthClient(client, {
      accessTokenStorage,
    });

    await authClient.logIn({
      email: "test@example.com",
      password: "password",
    });
    accessTokenStorage.delete("accessToken")

    const refreshResponse = await authClient.refresh();
    expect(refreshResponse).toBe(true)

    expect(accessTokenStorage.size).toBe(1);
    expect(accessTokenStorage.has("accessToken")).toBeTruthy();
  })
})
