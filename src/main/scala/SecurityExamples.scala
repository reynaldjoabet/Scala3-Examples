import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.file.Files
import java.security
import java.security.cert
import java.security.cert.CertPath
import java.security.cert.CertificateFactory
import java.security.cert.PKIXCertPathChecker
import java.security.cert.X509Certificate
import java.security.cert.X509Extension
import java.security.interfaces.DSAPrivateKey
import java.security.interfaces.ECPrivateKey
import java.security.interfaces.EdECPublicKey
import java.security.interfaces.RSAPrivateKey
import java.security.interfaces.RSAPublicKey
import java.security.spec.DSAPublicKeySpec
import java.security.spec.X509EncodedKeySpec
import java.security.Certificate
import java.security.KeyFactory
import java.security.KeyPairGenerator
import java.security.KeyStore
import java.security.MessageDigest
import java.security.PrivateKey
import java.security.PublicKey
import java.security.SecureRandom
import java.security.Security
import java.security.Signature

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
//import java.util.Random
import scala.util.Random
import scala.util.Try

import com.sun.crypto.provider
import com.sun.crypto.provider.AESCipher
import com.sun.crypto.provider.AESKeyGenerator
import com.sun.crypto.provider.AESParameters
import com.sun.crypto.provider.ARCFOURCipher
import com.sun.crypto.provider.BlowfishCipher
import com.sun.crypto.provider.BlowfishKeyGenerator
import com.sun.crypto.provider.ChaCha20Cipher
import com.sun.crypto.provider.DESCipher
import com.sun.crypto.provider.DESKeyFactory
import com.sun.crypto.provider.DESParameters
import com.sun.crypto.provider.DESedeCipher
import com.sun.crypto.provider.DHKeyAgreement
import com.sun.crypto.provider.DHKeyFactory
import com.sun.crypto.provider.DHKeyPairGenerator
import com.sun.crypto.provider.DHParameterGenerator
import com.sun.crypto.provider.HmacCore
import com.sun.crypto.provider.JceKeyStore
import com.sun.crypto.provider.KeyGeneratorCore
import com.sun.crypto.provider.KeyWrapCipher
import com.sun.crypto.provider.PBEKeyFactory
import com.sun.crypto.provider.PKCS12PBECipherCore
import com.sun.crypto.provider.RC2Parameters
import com.sun.crypto.provider.RSACipher
import com.sun.crypto.provider.SslMacCore
//import javax.security.cert.X509Certificate
//import javax.security.cert.X509Certificate
import com.sun.crypto.provider.SunJCE
import com.sun.crypto.provider.TlsKeyMaterialGenerator
import com.sun.crypto.provider.TlsMasterSecretGenerator
import com.sun.crypto.provider.TlsPrfGenerator
import com.sun.crypto.provider.TlsRsaPremasterSecretGenerator
import com.sun.org.slf4j.internal.Logger
import com.sun.security
import javax.crypto
import javax.crypto.spec
import javax.crypto.spec.ChaCha20ParameterSpec
import javax.crypto.spec.DESKeySpec
import javax.crypto.spec.DESedeKeySpec
import javax.crypto.spec.DHGenParameterSpec
import javax.crypto.spec.DHPrivateKeySpec
import javax.crypto.spec.DHPublicKeySpec
import javax.crypto.spec.GCMParameterSpec
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.OAEPParameterSpec
import javax.crypto.spec.PBEKeySpec
import javax.crypto.spec.PBEParameterSpec
import javax.crypto.spec.RC2ParameterSpec
import javax.crypto.spec.RC5ParameterSpec
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Cipher
import javax.crypto.KeyGenerator
import javax.crypto.SecretKeyFactory
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.TrustManagerFactory

object SecurityExamples extends App {

  given name: Int = 9

  val ctx = summon[ExecutionContext]

  val generator = KeyPairGenerator.getInstance("RSA");
  generator.initialize(2048);
  val keyGen = KeyGenerator.getInstance("Blowfish")
  val pair   = generator.generateKeyPair();

  val privateKey: PrivateKey = pair.getPrivate();
  val publicKey: PublicKey   = pair.getPublic();

  Try(new FileOutputStream("public.pem")).map(_.write(publicKey.getEncoded()))
  val publicKeyFile  = new File("public.pem");
  val publicKeyBytes = Files.readAllBytes(publicKeyFile.toPath());
  println(privateKey.getEncoded())
  println(privateKey.getFormat())
  println(privateKey.getAlgorithm())
//PKIXCertPathChecker
  import javax.security.auth.x500.X500Principal
  println(publicKey.getEncoded())
  println(publicKey.getFormat())
  println(publicKey.getAlgorithm())

  println(publicKeyBytes)

  val keyFactory: KeyFactory = KeyFactory.getInstance("RSA")

  val keyManagerFactory =
    KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm())

  val trustManagerFactory =
    TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())

  val publicKeySpec = new X509EncodedKeySpec(publicKeyBytes)
  keyFactory.generatePublic(publicKeySpec)
  val secure = SecureRandom.getInstance("NativePRNG")

  val secure1 = SecureRandom()
  // DSAPublicKeySpec
  // DESKeySpec
  val home = System.getProperty("java.home")

//println(home)
//sys.props.foreach(println)
//sys.getProperties().forEach(c=>println(c))
  val cipher: Cipher =
    Cipher.getInstance("RSA/ECB/OAEPWITHSHA-512ANDMGF1PADDING", "SunJCE")

  val cipher2: Cipher = Cipher.getInstance("Blowfish", "SunJCE")

  val cipher3: Cipher =
    Cipher.getInstance("RSA/ECB/OAEPWITHSHA-512/256ANDMGF1PADDING", "SunJCE")

//val signature: Signature=Signature.getInstance("SHA256withRSA","SunJCE")
  val signature2: Signature =
    Signature.getInstance("SHA256withRSA", "SunRsaSign")

//signature.initVerify()
  val signature3: Signature = Signature.getInstance("SHA256withRSA")
  val signature4: Signature = Signature.getInstance("SHA3-256withRSA")
  val signature5: Signature = Signature.getInstance("SHA3-384withRSA")
  val signature6: Signature = Signature.getInstance("SHA3-512withRSA")
//Security.getProviders().foreach(println)
//println(cipher.getProvider().getName().toString())

  val g = Try(new FileInputStream("fileName-of-cert")).map { inStream =>
    val cf   = CertificateFactory.getInstance("X.509");
    val cert = cf.generateCertificate(inStream).asInstanceOf[X509Certificate]
    cert.getIssuerX500Principal()
  }

  // val mac=MessageDigest.getInstance("HmacSHA3-512")
  val keystore = KeyStore.getInstance(KeyStore.getDefaultType())
  println(KeyStore.getDefaultType())
  val fileName = home + "/lib/security/cacerts".replace("/", File.separator)
  val is       = new FileInputStream(fileName)

  println(fileName)
  keystore.load(is, "changeit".toCharArray())

  println(keystore.getProvider())

  println(KeyManagerFactory.getDefaultAlgorithm())

  val keyGenerator = KeyGenerator.getInstance("AES")
  keyGenerator.init(192)
  val key = keyGen.generateKey()

  println(key.getAlgorithm())
  println(key.getEncoded())
  println(keyGenerator.getProvider())

  val li = (1 to 1000).toList

//li.sliding(4,1).foreach(println)
  val random  = new SecureRandom()
  val ivBytes = new Array[Byte](16)
  random.nextBytes(ivBytes)
  val iv       = new IvParameterSpec(ivBytes);
  val salt     = "passwordsalt".getBytes("UTF-8")
  val keySpec  = new PBEKeySpec("password".toCharArray(), salt, 65536, 256)
  val factory  = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
  val bytes    = factory.generateSecret(keySpec).getEncoded
  val sKeySpec = new SecretKeySpec(bytes, "AES")
  val eCipher  = Cipher.getInstance("AES/CBC/PKCS5Padding")
  eCipher.init(Cipher.ENCRYPT_MODE, sKeySpec, iv)
  val dCipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
  dCipher.init(Cipher.DECRYPT_MODE, sKeySpec, iv)

  println(
    iv.getIV()
      .foreach(
        println
      )
  )
  val km = KeyManagerFactory.getInstance("SunX509")
  // println(Random.nextString(20))
  // km.init(keystore)

}
