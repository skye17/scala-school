package lectures.functions

import org.scalatest.{GivenWhenThen, Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import AuthenticationData.{registeredCards, registeredLoginAndPassword}
import Authentication.{authByCard, authByLP, authFunction}

/**
  * Авторизация это очень важно, поэтому нам необходимо покрыть тестами ответсвенный за нее код
  * (lectures.functions.Authentication)
  *
  * Для этого
  * * * * уберите extends App у Authentication
  * * * * замените AuthenticationData.testUsers соответствующими генераторами
  * * * * напишите
  * * * * * 2 теста на authByCard
  * * * * * 2 теста на authByLP
  * * * * * 1 тест на их композицию
  *
  */
class AuthenticationTest extends WordSpec with Matchers with GivenWhenThen with PropertyChecks {
  val cardUserGen: Gen[CardUser] = Gen.zip(Gen.choose(0, Integer.MAX_VALUE),
    Gen.choose(0, Integer.MAX_VALUE)).map(p => CardUser(p._1, CardCredentials(p._2)))
  val lpUserGen: Gen[LPUser] = Gen.zip(Gen.choose(0, Integer.MAX_VALUE),
    Gen.alphaStr, Gen.alphaStr).map(p => LPUser(p._1, LPCredentials(p._2, p._3)))
  val anonymousUserGen = Gen.const(AnonymousUser())

  val registeredCardUserGen = Gen.zip(Gen.choose(0, Integer.MAX_VALUE),
    Gen.oneOf(registeredCards.toList)).map(p => CardUser(p._1, p._2))
  val registeredLPUserGen = Gen.zip(Gen.choose(0, Integer.MAX_VALUE),
    Gen.oneOf(registeredLoginAndPassword.toList)).map(p => LPUser(p._1, p._2))
  val registeredUserGen = Gen.oneOf(registeredCardUserGen, registeredLPUserGen)
  val userGen = Gen.oneOf(anonymousUserGen, cardUserGen, lpUserGen)


  "An user" when {
    "have registered card" should {
      "be successfully authenticated" in {
        forAll(registeredCardUserGen) { (user: User) => {
          (authByCard(user) == user) shouldBe true
        }
        }
      }
    }
    "doesn't have registered card" should {
      "cause a MatchError" in {
        val ms = generatorDrivenConfig.minSuccessful
        val notRegisteredCardUsers:Set[User] = (1 to ms).flatMap(_ => cardUserGen.sample).toList.
          filter((cu: CardUser) => !registeredCards.contains(cu.credentials)) toSet
        val otherUsers:Set[User] = (1 to ms).flatMap(_ => Gen.oneOf(lpUserGen, anonymousUserGen).sample) toSet
        val notRegisteredUsers = notRegisteredCardUsers union otherUsers
        for (user <- notRegisteredUsers) {
          intercept[MatchError]{ authByCard(user) }
        }
      }
    }

    "have login and password" should {
      "be successfully authenticated" in {
        forAll(registeredLPUserGen) { (user: User) =>
          (authByLP(user) == user) shouldBe true

        }
      }
    }

    "doesn't have login and password" should {
      "cause a MatchError" in {
        val ms = generatorDrivenConfig.minSuccessful
        val notRegisteredLPUsers:Set[User] = (1 to ms).flatMap(_ => lpUserGen.sample).toList.
          filter((lp: LPUser) => !registeredLoginAndPassword.contains(lp.credentials)) toSet
        val otherUsers:Set[User] = (1 to ms).flatMap(_ => Gen.oneOf(cardUserGen, anonymousUserGen).sample) toSet

        val notRegisteredUsers = notRegisteredLPUsers union otherUsers
        for (user <- notRegisteredUsers) {
          intercept[MatchError]{ authByLP(user) }
        }

      }
    }

    "have registered card or lp" should {
      "be successfully authenticated" in {
        forAll(registeredUserGen) { (user: User) => {
          authFunction(user).isDefined shouldBe true
        }
        }
      }

    }
  }
}

