export type ClientConfig = {
  baseUrl: string
}

export type FetchParameters = {
  route: string,
  searchParams?: URLSearchParams,
  method?: string,
  body?: object,
  headers?: Record<string, string>
}

export class Client {
  config: ClientConfig

  constructor(clientConfig: ClientConfig) {
    const validBaseUrl = clientConfig.baseUrl.endsWith('/') ? clientConfig.baseUrl.slice(0, -1)  :  clientConfig.baseUrl;
    this.config = {
      ...clientConfig,
      baseUrl: validBaseUrl
    };
  }

  async makeFetch<Result>({ route, searchParams, method, body, headers = {} }: FetchParameters): Promise<Result> {
    const parsedRoute = searchParams ? `${this.config.baseUrl}/${route}?${searchParams}` : `${this.config.baseUrl}/${route}`;

    const response = await fetch(parsedRoute, {
      method: method ?? "GET",
      headers: { "Content-Type": "application/json", ...headers },
      body: JSON.stringify(body)
    })

    if(!response.ok) {
      const error = await response.text();
      throw new Error(error);
    }

    return await response.json();
  }
}
