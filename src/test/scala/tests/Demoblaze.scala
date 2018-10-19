package tests

import io.gatling.core.Predef._
import io.gatling.core.structure.{ChainBuilder, ScenarioBuilder}
import io.gatling.http.Predef._
import io.gatling.http.protocol.HttpProtocolBuilder

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Demoblaze extends Simulation {

    object Action {
        val mainPage: ChainBuilder = group( "Main page" )(
            exec( session => {
                val idsInCart = ArrayBuffer.empty[String]
                session.set( "cookie", Func.generateCookie() )
                    .set( "generatedPId", Func.generateProdId() )
                    .set("idsInCart", idsInCart)
            } )
                .exec( http( "Get HTML" )
                    .get( Value.baseUrl )
                    .check( regex("""onClick="byCat\('([a-zA-Z]*)'\)""" )
                        .findAll
                        .saveAs( "categoryList" ) ) )
                .exec( http( "Get entries" )
                    .get( Value.apiCall + "/entries" )
                    .check( Func.checkJson( "$..img", "images" ) )
                    .check( Func.checkJson( "$..id", "ids" ) ) )
                .exec( Func.getImages( "${images}", "image" ) )
                .pause( Value.thinkTime / 2, Value.thinkTime * 2 )
        )

        val selectCategory: ChainBuilder = group( "Select category" )(
            exec( http( "OPTIONS request" )
                .options( Value.apiCall + "/bycat" )
                .check( headerRegex( "Access-Control-Allow-Methods", ".*POST.*" )
                    .find
                    .saveAs( "allowCategorization" ) ) )
                .doIf( "${allowCategorization.exists()}" ) {
                    exec( http( "POST Request" )
                        .post( Value.apiCall + "/bycat" )
                        .body( StringBody("""{"cat":"${categoryList.random()}"}""" ) )
                        .asJson
                        .check( Func.checkJson( "$..img", "images" ) )
                        .check( Func.checkJson( "$..id", "ids" ) ) )
                        .exec( Func.getImages( "${images}", "image" ) )
                        .pause( Value.thinkTime )
                }
        )

        val changePage: ChainBuilder = group( "Change page" )(
            exec( http( "OPTIONS Request" )
                .options( Value.apiCall + "/pagination" )
                .check( headerRegex( "Access-Control-Allow-Methods", ".*POST.*" )
                    .find
                    .saveAs( "allowPagination" ) ) )
                .doIf( "${allowPagination.exists()}" ) {
                    exec( http( "POST Request" )
                        .post( Value.apiCall + "/pagination" )
                        .body( StringBody("""{"id":"""" + Func.chooseAny( Value.paginationKeys ) + """"}""" ) )
                        .asJson
                        .check( Func.checkJson( "$..img", "images" ) )
                        .check( Func.checkJson( "$..id", "ids" ) ) )
                        .exec( Func.getImages( "${images}", "image" ) )
                        .pause( Value.thinkTime )
                }
        )

        val viewProduct: ChainBuilder = group( "View product" )(
            doIf( "${ids.exists()}" ) {
                exec( session => {
                    val ids = session( "ids" ).as[Vector[String]]
                    val random = Random.nextInt( ids.length )
                    session.set( "currentId", ids( random ) )
                } )
                    .exec( http( "Get HTML" )
                        .get( Value.baseUrl + "/prod.html" )
                        .queryParam( "idp_", "${currentId}" ) )
                    .exec( http( "OPTIONS Request" )
                        .options( Value.apiCall + "/view" )
                        .check( headerRegex( "Access-Control-Allow-Methods", ".*POST.*" )
                            .find
                            .saveAs( "allowViewing" ) ) )
                    .doIf( "${allowViewing.exists()}" ) {
                        exec( http( "POST Request" )
                            .post( Value.apiCall + "/view" )
                            .body( StringBody("""{id: "${currentId}"}""" ) )
                            .asJson )
                    }
                    .pause( Value.thinkTime )
            }
        )

        val addToCart: ChainBuilder = group( "Add to cart" )(
            doIf( "${currentId.exists()}" ) {
                exec( http( "Send OPTIONS" )
                    .options( Value.apiCall + "/addtocart" )
                    .check( headerRegex( "Access-Control-Allow-Methods", ".*POST.*" )
                        .find
                        .saveAs( "allowAdding" ) ) )
                    .doIf( "${allowAdding.exists()}" ) {
                        exec( http( "Send POST" )
                            .post( Value.apiCall + "/addtocart" )
                            .body( StringBody(
                                """{"id":"${generatedPId}",
                                  |"cookie":"${cookie}",
                                  |"prod_id":${currentId},
                                  |"flag":false}""".stripMargin ) )
                            .asJson )
                            .exec( session => {
                                session("idsInCart").as[ArrayBuffer[String]] += session("currentId").as[String]
                                session
                            } )

                    }
            }
                .pause( Value.thinkTime/2, Value.thinkTime*2 )
        )

        val viewCart: ChainBuilder = group( "View cart" )(
            exec( http( "Get HTML" )
                .get( Value.baseUrl + "/cart.html" ) )
                .exec( http( "Send OPTIONS" )
                    .options( Value.apiCall + "/viewcart" )
                    .check( headerRegex( "Access-Control-Allow-Methods", ".*POST.*" )
                        .find
                        .saveAs( "allowViewingCart" ) ) )
                .doIf( "${allowViewingCart.exists()}" ) {
                    exec( http( "Send POST" )
                        .post( Value.apiCall + "/viewcart" )
                        .body( StringBody("""{"cookie":"${cookie}","flag":false}""" ) )
                        .asJson)
                        .exec( Func.postIds( "${idsInCart}" ) )
                }
                .pause( Value.thinkTime )
        )

        val purchase: ChainBuilder = group( "Purchase" )(
            exec( http( "Send OPTIONS" )
                .options( Value.apiCall + "/deletecart" )
                .check( headerRegex( "Access-Control-Allow-Methods", ".*POST.*" )
                    .find
                    .saveAs( "allowPurchasing" ) ) )
                .doIf( "${allowPurchasing.exists()}" ) {
                    exec( http( "Send POST" )
                        .post( Value.apiCall + "/deletecart" )
                        .body( StringBody( """{"cookie":"${cookie}"}""" ) )
                        .asJson
                        .check( substring( "null" ) ) )
                        .exec( http( "Redirect to Main" )
                            .get( Value.baseUrl ) )
                }
                .pause( Value.thinkTime )
        )
    }

    object Func {
        val generateHex = () => (1 + Random.nextDouble() * 0x10000).toInt.toHexString
        val generateProdId = () => generateHex() + generateHex() + generateHex()
        val generateCookie = () => "user=" + generateHex() + generateHex() + "-" + generateHex() + "-" +
            generateHex() + "-" + generateHex() + "-" + generateHex() + generateHex() + generateHex()
        val checkJson = (pathPattern: String, saveTo: String) => {
            checkBuilder2HttpCheck( jsonPath( pathPattern )
                .findAll
                .saveAs( saveTo ) )
        }
        val getImages = (extractFrom: String, attrName: String) => {
            foreach( extractFrom, attrName ) {
                exec(
                    http( "Get images" )
                        .get( Value.baseUrl + "/${image}" ) )
            }
        }
        val postIds = (extractFrom: String) => {
            exec( foreach( extractFrom, "id" ) {
                exec( http( "Send OPTIONS" )
                    .options( Value.apiCall + "/view" ) )
            } )
                .exec( foreach( extractFrom, "id" ) {
                    exec( http( "Send POST" )
                        .post( Value.apiCall + "/view" )
                        .body( StringBody( "{id: " + "${id}" + "}" ) )
                        .asJson )
                } )
        }
        val chooseAny = (x: Seq[Any]) => x( Random.nextInt( x.length ) )
    }

    object Value {
        val paginationKeys = Vector( 4, 7, 13 )
        val baseUrl = "http://www.demoblaze.com"
        val apiCall = "https://api.demoblaze.com"
        val thinkTime = 20
        val proxyHost = "localhost"
        val proxyPort = 8888
        val httpProtocol: HttpProtocolBuilder = http
            .baseUrl( baseUrl )
            .inferHtmlResources( WhiteList( baseUrl + "/.*", apiCall + "/.*" ) )
            .nameInferredHtmlResources( _ => "Get embed" )
            .maxConnectionsPerHostLikeOpera
            .check( status.in( 200, 304 ) )
            .acceptEncodingHeader( "gzip, deflate" )
            .userAgentHeader( "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 " +
                "(KHTML, like Gecko) Chrome/69.0.3497.100 Safari/537.36 OPR/56.0.3051.36" )
            .proxy(
                Proxy( proxyHost, proxyPort )
                    .httpsPort( proxyPort )
            )
    }

    object Scenario {
        val users = 10
        val rampDuration = 10
        val holdLoad = 240
        val navigate: ScenarioBuilder = scenario( "Navigate" )
            .exec(
                Action.mainPage,
                repeat( 1 + Random.nextInt( 3 ) ) {
                    randomSwitch(
                        100/3d -> Action.selectCategory,
                        100/3d -> Action.changePage,
                        100/3d -> Action.viewProduct)
                }
            )

        val purchase: ScenarioBuilder = scenario( "Purchase" )
            .exec(
                repeat( 1 + Random.nextInt( 3 ) ) {
                    exec( Action.viewProduct, Action.addToCart)},
                Action.viewCart,
                Action.purchase
            )

        val combined: ScenarioBuilder = scenario( "Demoblaze" )
            .forever(
                randomSwitch(
                    90d -> exec( navigate )
                        .exec( flushHttpCache ),
                    10d -> exec( navigate )
                        .exec( purchase )
                        .exec( flushHttpCache )
                )
        )
    }

    setUp(
        Scenario.combined.inject(
            constantUsersPerSec(1) during 1
        ))
        .protocols( Value.httpProtocol )
        .maxDuration( Scenario.holdLoad )
}
