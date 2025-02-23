import { assert, lazy, object, size, string } from "superstruct"
import { Client } from "../client"
import { LoginResponse, RegisterResponse } from "../types/responses"

export type RegisterData = {
  email: string,
  username: string,
  password: string,
  retypedPassword: string
}

export type LoginData = {
  email: string,
  password: string,
}

export type TokenStore = {
  set: (key: string, value: string) => void,
  get: (key: string) => string | null | undefined,
  delete: (key: string) => void,
}

export type AuthClientConfig = {
  accessTokenStorage: TokenStore,
}

export class AuthClient {
  client: Client
  config: AuthClientConfig

  constructor(client: Client, config: AuthClientConfig) {
    this.client = client;
    this.config = config; 
  }

  async register(body : RegisterData): Promise<string | boolean> {

    const RegisterDataValidator = object({
      email: size(string(), 1, 255),
      username: size(string(), 1, 50),
      password: size(string(), 8),
      retypedPassword: size(string(), 8)
    })

    assert(body, RegisterDataValidator);

    const response = await this.client.makeFetch<RegisterResponse>({
      route: "auth/register",
      method: "POST",
      body: body
    });

    return response.message === "Success!" ? true : response.message;
  }

  async logIn(body: LoginData): Promise<string | boolean> {
    const RegisterDataValidator = object({
      email: size(string(), 1, 255),
      password: size(string(), 8)
    })

    assert(body, RegisterDataValidator);

    const response = await this.client.makeFetch<LoginResponse>({
      route: "auth/login",
      method: "POST",
      body: body
    });
    
    if(response.token) {
      this.config.accessTokenStorage.set("accessToken", response.token)
      return true;
    }

    return response.message;
  }
}
