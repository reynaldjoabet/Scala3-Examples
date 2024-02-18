import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import javax.crypto.spec.IvParameterSpec
import java.security.SecureRandom
val random  = new SecureRandom()
val ivBytes = new Array[Byte](16)
random.nextBytes(ivBytes)
/**
 * This class specifies an <i>initialization vector</i> (IV).
 * Examples which use IVs are ciphers in feedback mode,
 * e.g., DES in CBC mode and RSA ciphers with OAEP encoding
 * operation.
 * */
val iv       = new IvParameterSpec(ivBytes);
val salt     = "salt".getBytes("UTF-8")
//A user-chosen password that can be used with password-based encryption
val keySpec  = new PBEKeySpec("password".toCharArray(), salt, 655360, 256)
//This class represents a factory for secret keys.
//Secret key factories operate only on secret (symmetric) keys
val factory  = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
val bytes    = factory.generateSecret(keySpec).getEncoded
val sKeySpec = new SecretKeySpec(bytes, "AES")

val eCipher  = Cipher.getInstance("AES/CBC/PKCS5Padding")
eCipher.init(Cipher.ENCRYPT_MODE, sKeySpec, iv)
val dCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
dCipher.init(Cipher.DECRYPT_MODE, sKeySpec, iv)