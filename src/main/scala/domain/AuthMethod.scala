package domain


enum AuthMethod(val value: String) derives CanEqual{

  case None extends AuthMethod("none")
  // OpenID Connect clientSecretBasic
  case SecretBasic extends AuthMethod("secret_basic")

  case ClientSecretBasic extends AuthMethod("client_secret_basic")
  // OpenID Connect ClientSecretPost
  case SecretPost extends AuthMethod("secret_post")

  case ClientSecretPost extends AuthMethod("client_secret_post")

  case ClientSecretJwt extends AuthMethod("client_secret_jwt")

  case PrivateKeyJwt extends AuthMethod("private_key_jwt")
}
