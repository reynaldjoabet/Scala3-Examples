package domain

enum ResponseType(val value: String) derives CanEqual{
  case Code extends ResponseType("code")
  
  case Token extends ResponseType("token")
}
