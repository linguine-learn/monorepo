type BaseResponse = {
  message: string,
}
export type RegisterResponse = {
} & BaseResponse;

export type LoginResponse = {
  token: string | null,
} & BaseResponse;
