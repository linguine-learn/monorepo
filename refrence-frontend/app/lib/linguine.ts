import * as Linguine from "@linguine/client-sdk";

export const linguineClient = new Linguine.Client({
  baseUrl: process.env.EXPO_PUBLIC_LINGUINE_API ?? "http://localhost:3000",
});
export const linguineAuthClient = new Linguine.AuthClient(linguineClient, {
  accessTokenStorage: new Map(),
});
