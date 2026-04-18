#' Add file csl GOST style
#'
#' @param folder_name Way to file
#'
#'
#' @return Append file hlper.R
#' @export
#' @import glue
#'


add_gost_csl <- function(folder_name){
    csl_file <- glue("{folder_name}ama.csl")
    file.create(csl_file)

    cat("<?xml version=\"1.0\" encoding=\"utf-8\"?>
<style xmlns=\"http://purl.org/net/xbiblio/csl\" version=\"1.0\" class=\"in-text\" default-locale=\"ru-RU\">
  <info>
    <title>Russian GOST R 7.0.5-2008 (numeric, sorted alphabetically, Ру́сский)</title>
    <title-short>GOST R 7.0.5-2008 (numeric, sorted alphabetically)</title-short>
    <id>http://www.zotero.org/styles/gost-r-7-0-5-2008-numeric-alphabetical</id>
    <link href=\"http://www.zotero.org/styles/gost-r-7-0-5-2008-numeric-alphabetical\" rel=\"self\"/>
    <link href=\"http://protect.gost.ru/document.aspx?control=7&amp;id=173511\" rel=\"documentation\"/>
    <link href=\"http://standartgost.ru/g/%D0%93%D0%9E%D0%A1%D0%A2_%D0%A0_7.0.5-2008\" rel=\"documentation\"/>
    <author>
      <name>Rybakov Dmitriy</name>
    </author>
    <category citation-format=\"numeric\"/>
    <category field=\"generic-base\"/>
    <summary>Russian GOST-2008 style. Book, report, webpage, post-weblog and article chapters edited. Note!!! In articles form of № = Issue (Volume).</summary>
    <updated>2021-04-23T06:08:18+00:00</updated>
    <rights license=\"http://creativecommons.org/licenses/by-sa/3.0/\">This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 License</rights>
  </info>
  <locale xml:lang=\"ru\">
    <terms>
      <term name=\"et-al\"> </term>
      <term name=\"editor\">под ред.</term>
      <term name=\"accessed\">дата обращения</term>
      <term name=\"page\" form=\"short\">с.</term>
      <term name=\"translator\">перевод</term>
    </terms>
  </locale>
  <locale xml:lang=\"en\">
  <terms>
    <term name=\"et-al\">[et al.]</term>
    <term name=\"page\" form=\"short\">p.</term>
    <term name=\"volume\" form=\"short\">Vol.</term>  <!-- Это важно! -->
    <term name=\"issue\" form=\"short\">№</term>
  </terms>
</locale>
  <macro name=\"editor\">
    <names variable=\"editor\" delimiter=\", \" suffix=\",\">
      <label prefix=\" \" suffix=\" \"/>
      <name initialize-with=\". \"/>
    </names>
  </macro>
  <macro name=\"author\">
    <names variable=\"author\" delimiter=\",\" prefix=\" / \" suffix=\",\">
      <name et-al-min=\"1\" et-al-use-first=\"3\" initialize-with=\". \"/>
      <label form=\"short\" strip-periods=\"true\"/>
    </names>
  </macro>
  <macro name=\"access\">
    <group>
      <text value=\"[Электронный ресурс]. \"/>
      <text variable=\"URL\" prefix=\"URL: \"/>
      <group prefix=\" (\" suffix=\").\">
        <text term=\"accessed\" suffix=\": \"/>
        <date variable=\"accessed\">
          <date-part name=\"day\" suffix=\".\" form=\"numeric-leading-zeros\"/>
          <date-part name=\"month\" suffix=\".\" form=\"numeric-leading-zeros\"/>
          <date-part name=\"year\"/>
        </date>
      </group>
    </group>
  </macro>
  <macro name=\"title\">
    <text variable=\"title\"/>
  </macro>
  <macro name=\"translator\">
    <names variable=\"translator\" delimiter=\",\" suffix=\",\">
      <label prefix=\" \" suffix=\" \"/>
      <name initialize-with=\". \"/>
    </names>
  </macro>
  <macro name=\"publisher\">
    <group suffix=\",\">
      <text variable=\"publisher-place\" suffix=\":\"/>
      <text variable=\"publisher\" prefix=\" \"/>
    </group>
    <text macro=\"year-date\" prefix=\" \" suffix=\".\"/>
  </macro>
  <macro name=\"year-date\">
    <date variable=\"issued\">
      <date-part name=\"year\"/>
    </date>
  </macro>
  <macro name=\"language-sort\">
    <text variable=\"note\"/>
    <text value=\"Z999\"/>
  </macro>
  <macro name=\"citation-number\">
    <text variable=\"citation-number\" suffix=\". \"/>
  </macro>
  <macro name=\"sort-key\">
    <group delimiter=\" \">
      <!-- Фамилия первого автора для сортировки -->
      <names variable=\"author\">
        <name name-as-sort-order=\"first\" 
              form=\"long\" 
              sort-separator=\", \"
              names-min=\"1\"
              names-use-first=\"1\"/>
      </names>
      <!-- Название для сортировки внутри одного автора -->
      <text variable=\"title\"/>
    </group>
  </macro>

  <!-- МАКРОС ДЛЯ ОТОБРАЖЕНИЯ ПЕРВОГО АВТОРА (Фамилия, И.О.) -->
  <macro name=\"author-first\">
    <names variable=\"author\">
      <name name-as-sort-order=\"first\" 
            sort-separator=\", \" 
            initialize-with=\".\" 
            form=\"long\"
            names-min=\"1\"
            names-use-first=\"1\"/>
    </names>
  </macro>
  <macro name=\"author-after-slash\">
      <names variable=\"author\">
        <name and=\"text\" 
              delimiter=\", \" 
              delimiter-precedes-last=\"always\"
              initialize-with=\".\" 
              form=\"long\" 
              et-al-min=\"4\" 
              et-al-use-first=\"3\">
          <name-part name=\"family\" prefix=\" \"/>
          <name-part name=\"given\" suffix=\" \" prefix=\"\"/>
        </name>
      </names>
    </macro>
  <citation collapse=\"citation-number\">
    <sort>
      <key variable=\"citation-number\"/>
    </sort>
    <layout prefix=\"[\" suffix=\"]\" delimiter=\", \">
      <text variable=\"citation-number\"/>
      <group prefix=\",\">
        <label plural=\"never\" prefix=\" \" variable=\"locator\" form=\"short\"/>
        <text variable=\"locator\" form=\"short\" prefix=\" \"/>
      </group>
    </layout>
  </citation>
  <bibliography et-al-min=\"4\" et-al-use-first=\"1\">
    <sort>
      <key macro=\"language-sort\"/>
      <key macro=\"author-first\"/>
      <key variable=\"issued\"/>
    </sort>
    <layout suffix=\".\">
      <text macro=\"citation-number\"/>
      <text macro=\"author-first\" suffix=\" \"/>
      <choose>
        <if type=\"book\" match=\"any\">
          <group>
            <group delimiter=\" \">
              <text macro=\"title\"/>
              <text macro=\"author\"/>
              <text macro=\"editor\"/>
              <text macro=\"translator\"/>
              <text variable=\"edition\" suffix=\"-е изд.,\"/>
            </group>
            <text macro=\"publisher\" prefix=\" \"/>
            <text variable=\"number-of-pages\" prefix=\" \" suffix=\" c.\"/>
          </group>
        </if>
        <else-if type=\"chapter paper-conference\" match=\"any\">
          <group>
            <group delimiter=\" \">
              <text macro=\"title\"/>
              <text variable=\"collection-title\" suffix=\" /\"/>
              <text macro=\"editor\"/>
            </group>
            <text macro=\"publisher\" prefix=\" \"/>
            <text variable=\"page\" prefix=\"C. \" suffix=\".\"/>
          </group>
        </else-if>
        <else-if type=\"webpage post-weblog\" match=\"any\">
          <text macro=\"title\"/>
          <text variable=\"container-title\" prefix=\" // \"/>
          <text prefix=\" \" macro=\"access\"/>
        </else-if>
        <else-if type=\"article-journal article-magazine article-newspaper\" match=\"any\">
          <text macro=\"title\" suffix=\". \"/>
          <text value=\"/ \" prefix=\"\"/>
          <text macro=\"author-after-slash\"/>
          <choose>
            <if variable=\"note\" match=\"russian|ru\">
              <text value=\" [и др.]\"/>
            </if>
            <else>
              <text value=\" [et al.]\"/>
            </else>
          </choose>
          <text value=\" // \" prefix=\"\"/>
          <text variable=\"container-title\" suffix=\". - \"/>
          
          <!-- ГОД: для русских с \"г.\", для английских без -->
          <choose>
            <if variable=\"note\" match=\"russian|ru\">
              <date variable=\"issued\" form=\"text\" date-parts=\"year\" suffix=\" - \"/>
            </if>
            <else>
              <date variable=\"issued\" form=\"text\" date-parts=\"year\" suffix=\" - \"/>
            </else>
          </choose>
          
          <!-- ВЫПУСК: для русских \"вып.\", для английских \"Vol.\" -->
          <choose>
            <if variable=\"langid\" match=\"russian|ru\">
              <text term=\"volume\" form=\"short\" suffix=\"\"/>  <!-- вып. -->
            </if>
            <else>
              <text term=\"volume\" form=\"short\" suffix=\"\"/>  <!-- Vol. -->
            </else>
          </choose>
          <text variable=\"volume\" suffix=\". - \"/>
          
          <!-- СТРАНИЦЫ: для русских \"с.\", для английских \"p.\" -->
          <choose>
            <if variable=\"note\" match=\"russian|ru\">
              <text term=\"page\" form=\"short\" suffix=\"\"/>    <!-- с. -->
            </if>
            <else>
              <text term=\"page\" form=\"short\" suffix=\"\"/>    <!-- p. -->
            </else>
          </choose>
          <text variable=\"page\" suffix=\".\"/>
        </else-if>
        <else>
          <text macro=\"title\" prefix=\" \"/>
          <group delimiter=\". \" prefix=\" \" suffix=\".\">
            <group prefix=\"// \" delimiter=\". \">
              <text variable=\"container-title\"/>
              <text macro=\"year-date\"/>
              <text variable=\"volume\" prefix=\"Т. \"/>
              <text variable=\"issue\" prefix=\"№ \"/>
            </group>
            <text variable=\"page\" prefix=\"C. \" suffix=\".\"/>
          </group>
        </else>
      </choose>
    </layout>
  </bibliography>
</style>
", file =csl_file)

print("For correct sorting, you need to add \"note = {ru}\" to the file.bib for Russian-language sources.")
}
