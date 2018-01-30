<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xmlns:csc="urn:mace:funet.fi:julkaisut/2015/03/01">

	<xsl:output method="text" encoding="UTF-8"/>

	<xsl:template match="/">
<xsl:for-each select="//csc:Julkaisu[csc:DOI]">
<xsl:value-of select="csc:DOI"/>|<xsl:value-of select="csc:JulkaisuVuosi"/>|<xsl:value-of select="translate(csc:JulkaisunNimi, ';', '.')"/>|<xsl:value-of select="csc:LehdenNimi"/>|<xsl:value-of select="csc:TekijoidenLkm"/>|<xsl:value-of select="csc:AvoinSaatavuusKoodi"/>|<xsl:value-of select="csc:JulkaisunOrgYksikot/csc:YksikkoKoodi"/>|<xsl:value-of select="count(csc:TieteenalaKoodit/csc:TieteenalaKoodi)"/>|<xsl:value-of select="csc:TieteenalaKoodit/csc:TieteenalaKoodi"/><xsl:text>
</xsl:text></xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
