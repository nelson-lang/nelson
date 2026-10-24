//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4190)
#endif
//=============================================================================
#include "NelsonGateway.hpp"
#include "betacdfBuiltin.hpp"
#include "betafitBuiltin.hpp"
#include "betainvBuiltin.hpp"
#include "betalikeBuiltin.hpp"
#include "betapdfBuiltin.hpp"
#include "betarndBuiltin.hpp"
#include "betastatBuiltin.hpp"
#include "binocdfBuiltin.hpp"
#include "binofitBuiltin.hpp"
#include "binoinvBuiltin.hpp"
#include "binolikeBuiltin.hpp"
#include "binopdfBuiltin.hpp"
#include "binorndBuiltin.hpp"
#include "binostatBuiltin.hpp"
#include "chi2cdfBuiltin.hpp"
#include "chi2invBuiltin.hpp"
#include "chi2pdfBuiltin.hpp"
#include "chi2rndBuiltin.hpp"
#include "chi2statBuiltin.hpp"
#include "clusterBuiltin.hpp"
#include "corrcoefBuiltin.hpp"
#include "dbscanBuiltin.hpp"
#include "expcdfBuiltin.hpp"
#include "evcdfBuiltin.hpp"
#include "evfitBuiltin.hpp"
#include "evinvBuiltin.hpp"
#include "evlikeBuiltin.hpp"
#include "evpdfBuiltin.hpp"
#include "evrndBuiltin.hpp"
#include "evstatBuiltin.hpp"
#include "expfitBuiltin.hpp"
#include "expinvBuiltin.hpp"
#include "explikeBuiltin.hpp"
#include "exppdfBuiltin.hpp"
#include "exprndBuiltin.hpp"
#include "expstatBuiltin.hpp"
#include "fcdfBuiltin.hpp"
#include "finvBuiltin.hpp"
#include "fpdfBuiltin.hpp"
#include "frndBuiltin.hpp"
#include "fstatBuiltin.hpp"
#include "gamcdfBuiltin.hpp"
#include "gamfitBuiltin.hpp"
#include "gaminvBuiltin.hpp"
#include "gamlikeBuiltin.hpp"
#include "gampdfBuiltin.hpp"
#include "gamrndBuiltin.hpp"
#include "gamstatBuiltin.hpp"
#include "geocdfBuiltin.hpp"
#include "geofitBuiltin.hpp"
#include "geoinvBuiltin.hpp"
#include "geolikeBuiltin.hpp"
#include "geopdfBuiltin.hpp"
#include "georndBuiltin.hpp"
#include "geostatBuiltin.hpp"
#include "gevcdfBuiltin.hpp"
#include "gevfitBuiltin.hpp"
#include "gevinvBuiltin.hpp"
#include "gevlikeBuiltin.hpp"
#include "gevpdfBuiltin.hpp"
#include "gevrndBuiltin.hpp"
#include "gevstatBuiltin.hpp"
#include "kmeansBuiltin.hpp"
#include "kstestBuiltin.hpp"
#include "kstest2Builtin.hpp"
#include "knnsearchBuiltin.hpp"
#include "iqrBuiltin.hpp"
#include "linkageBuiltin.hpp"
#include "varBuiltin.hpp"
#include "meanBuiltin.hpp"
#include "stdBuiltin.hpp"
#include "logncdfBuiltin.hpp"
#include "lognfitBuiltin.hpp"
#include "logninvBuiltin.hpp"
#include "lognlikeBuiltin.hpp"
#include "lognpdfBuiltin.hpp"
#include "lognrndBuiltin.hpp"
#include "lognstatBuiltin.hpp"
#include "normcdfBuiltin.hpp"
#include "normfitBuiltin.hpp"
#include "norminvBuiltin.hpp"
#include "normlikeBuiltin.hpp"
#include "normrndBuiltin.hpp"
#include "normstatBuiltin.hpp"
#include "nbincdfBuiltin.hpp"
#include "nbinfitBuiltin.hpp"
#include "nbininvBuiltin.hpp"
#include "nbinlikeBuiltin.hpp"
#include "nbinpdfBuiltin.hpp"
#include "nbinrndBuiltin.hpp"
#include "nbinstatBuiltin.hpp"
#include "pdistBuiltin.hpp"
#include "poisscdfBuiltin.hpp"
#include "poissfitBuiltin.hpp"
#include "poissinvBuiltin.hpp"
#include "poisslikeBuiltin.hpp"
#include "poisspdfBuiltin.hpp"
#include "poissrndBuiltin.hpp"
#include "poissstatBuiltin.hpp"
#include "prctileBuiltin.hpp"
#include "quantileBuiltin.hpp"
#include "raylcdfBuiltin.hpp"
#include "raylfitBuiltin.hpp"
#include "raylinvBuiltin.hpp"
#include "rayllikeBuiltin.hpp"
#include "raylpdfBuiltin.hpp"
#include "raylrndBuiltin.hpp"
#include "raylstatBuiltin.hpp"
#include "rangesearchBuiltin.hpp"
#include "squareformBuiltin.hpp"
#include "ttestBuiltin.hpp"
#include "ttest2Builtin.hpp"
#include "tcdfBuiltin.hpp"
#include "tinvBuiltin.hpp"
#include "tpdfBuiltin.hpp"
#include "trndBuiltin.hpp"
#include "tstatBuiltin.hpp"
#include "unifcdfBuiltin.hpp"
#include "unifitBuiltin.hpp"
#include "unifinvBuiltin.hpp"
#include "uniflikeBuiltin.hpp"
#include "unifpdfBuiltin.hpp"
#include "unifrndBuiltin.hpp"
#include "unifstatBuiltin.hpp"
#include "unidcdfBuiltin.hpp"
#include "unidfitBuiltin.hpp"
#include "unidinvBuiltin.hpp"
#include "unidlikeBuiltin.hpp"
#include "unidpdfBuiltin.hpp"
#include "unidrndBuiltin.hpp"
#include "unidstatBuiltin.hpp"
#include "zscoreBuiltin.hpp"
#include "ztestBuiltin.hpp"
#include "vartestBuiltin.hpp"
#include "vartest2Builtin.hpp"
#include "wblcdfBuiltin.hpp"
#include "wblfitBuiltin.hpp"
#include "wblinvBuiltin.hpp"
#include "wbllikeBuiltin.hpp"
#include "wblpdfBuiltin.hpp"
#include "wblrndBuiltin.hpp"
#include "wblstatBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const std::wstring gatewayName = L"statistics";
//=============================================================================
static const nlsGateway gateway[] = {
    {
        "corrcoef",
        (ptrBuiltin)Nelson::StatisticsGateway::corrcoefBuiltin,
        1,
        1,
    },
    { "var", (ptrBuiltin)Nelson::StatisticsGateway::varBuiltin, 2, 4 },
    { "mean", (ptrBuiltin)Nelson::StatisticsGateway::meanBuiltin, 1, 4 },
    { "std", (ptrBuiltin)Nelson::StatisticsGateway::stdBuiltin, -1, -1 },
    { "normcdf", (ptrBuiltin)Nelson::StatisticsGateway::normcdfBuiltin, 3, 6 },
    { "norminv", (ptrBuiltin)Nelson::StatisticsGateway::norminvBuiltin, 3, 5 },
    { "normrnd", (ptrBuiltin)Nelson::StatisticsGateway::normrndBuiltin, 1, -1 },
    { "normstat", (ptrBuiltin)Nelson::StatisticsGateway::normstatBuiltin, 2, 2 },
    { "normfit", (ptrBuiltin)Nelson::StatisticsGateway::normfitBuiltin, 4, 5 },
    { "normlike", (ptrBuiltin)Nelson::StatisticsGateway::normlikeBuiltin, 2, 4 },
    { "evpdf", (ptrBuiltin)Nelson::StatisticsGateway::evpdfBuiltin, 1, 3 },
    { "evcdf", (ptrBuiltin)Nelson::StatisticsGateway::evcdfBuiltin, 1, 4 },
    { "evinv", (ptrBuiltin)Nelson::StatisticsGateway::evinvBuiltin, 1, 3 },
    { "evrnd", (ptrBuiltin)Nelson::StatisticsGateway::evrndBuiltin, 1, -1 },
    { "evstat", (ptrBuiltin)Nelson::StatisticsGateway::evstatBuiltin, 2, 2 },
    { "evfit", (ptrBuiltin)Nelson::StatisticsGateway::evfitBuiltin, 2, 5 },
    { "evlike", (ptrBuiltin)Nelson::StatisticsGateway::evlikeBuiltin, 2, 4 },
    { "gevpdf", (ptrBuiltin)Nelson::StatisticsGateway::gevpdfBuiltin, 1, 4 },
    { "gevcdf", (ptrBuiltin)Nelson::StatisticsGateway::gevcdfBuiltin, 1, 5 },
    { "gevinv", (ptrBuiltin)Nelson::StatisticsGateway::gevinvBuiltin, 1, 4 },
    { "gevrnd", (ptrBuiltin)Nelson::StatisticsGateway::gevrndBuiltin, 1, -1 },
    { "gevstat", (ptrBuiltin)Nelson::StatisticsGateway::gevstatBuiltin, 2, 3 },
    { "gevfit", (ptrBuiltin)Nelson::StatisticsGateway::gevfitBuiltin, 2, 5 },
    { "gevlike", (ptrBuiltin)Nelson::StatisticsGateway::gevlikeBuiltin, 2, 4 },
    { "lognpdf", (ptrBuiltin)Nelson::StatisticsGateway::lognpdfBuiltin, 1, 3 },
    { "logncdf", (ptrBuiltin)Nelson::StatisticsGateway::logncdfBuiltin, 3, 6 },
    { "logninv", (ptrBuiltin)Nelson::StatisticsGateway::logninvBuiltin, 3, 5 },
    { "lognrnd", (ptrBuiltin)Nelson::StatisticsGateway::lognrndBuiltin, 1, -1 },
    { "lognstat", (ptrBuiltin)Nelson::StatisticsGateway::lognstatBuiltin, 2, 2 },
    { "lognfit", (ptrBuiltin)Nelson::StatisticsGateway::lognfitBuiltin, 2, 5 },
    { "lognlike", (ptrBuiltin)Nelson::StatisticsGateway::lognlikeBuiltin, 2, 4 },
    { "raylpdf", (ptrBuiltin)Nelson::StatisticsGateway::raylpdfBuiltin, 1, 2 },
    { "raylcdf", (ptrBuiltin)Nelson::StatisticsGateway::raylcdfBuiltin, 1, 3 },
    { "raylinv", (ptrBuiltin)Nelson::StatisticsGateway::raylinvBuiltin, 1, 2 },
    { "raylrnd", (ptrBuiltin)Nelson::StatisticsGateway::raylrndBuiltin, 1, -1 },
    { "raylstat", (ptrBuiltin)Nelson::StatisticsGateway::raylstatBuiltin, 2, 1 },
    { "raylfit", (ptrBuiltin)Nelson::StatisticsGateway::raylfitBuiltin, 2, 4 },
    { "rayllike", (ptrBuiltin)Nelson::StatisticsGateway::rayllikeBuiltin, 2, 4 },
    { "wblpdf", (ptrBuiltin)Nelson::StatisticsGateway::wblpdfBuiltin, 1, 3 },
    { "wblcdf", (ptrBuiltin)Nelson::StatisticsGateway::wblcdfBuiltin, 1, 4 },
    { "wblinv", (ptrBuiltin)Nelson::StatisticsGateway::wblinvBuiltin, 1, 3 },
    { "wblrnd", (ptrBuiltin)Nelson::StatisticsGateway::wblrndBuiltin, 1, -1 },
    { "wblstat", (ptrBuiltin)Nelson::StatisticsGateway::wblstatBuiltin, 2, 2 },
    { "wblfit", (ptrBuiltin)Nelson::StatisticsGateway::wblfitBuiltin, 2, 5 },
    { "wbllike", (ptrBuiltin)Nelson::StatisticsGateway::wbllikeBuiltin, 2, 4 },
    { "geopdf", (ptrBuiltin)Nelson::StatisticsGateway::geopdfBuiltin, 1, 2 },
    { "geocdf", (ptrBuiltin)Nelson::StatisticsGateway::geocdfBuiltin, 1, 3 },
    { "geoinv", (ptrBuiltin)Nelson::StatisticsGateway::geoinvBuiltin, 1, 2 },
    { "geornd", (ptrBuiltin)Nelson::StatisticsGateway::georndBuiltin, 1, -1 },
    { "geostat", (ptrBuiltin)Nelson::StatisticsGateway::geostatBuiltin, 2, 1 },
    { "geofit", (ptrBuiltin)Nelson::StatisticsGateway::geofitBuiltin, 2, 2 },
    { "geolike", (ptrBuiltin)Nelson::StatisticsGateway::geolikeBuiltin, 2, 2 },
    { "betapdf", (ptrBuiltin)Nelson::StatisticsGateway::betapdfBuiltin, 1, 3 },
    { "betacdf", (ptrBuiltin)Nelson::StatisticsGateway::betacdfBuiltin, 1, 4 },
    { "betainv", (ptrBuiltin)Nelson::StatisticsGateway::betainvBuiltin, 1, 3 },
    { "betarnd", (ptrBuiltin)Nelson::StatisticsGateway::betarndBuiltin, 1, -1 },
    { "betastat", (ptrBuiltin)Nelson::StatisticsGateway::betastatBuiltin, 2, 2 },
    { "betafit", (ptrBuiltin)Nelson::StatisticsGateway::betafitBuiltin, 2, 2 },
    { "betalike", (ptrBuiltin)Nelson::StatisticsGateway::betalikeBuiltin, 2, 2 },
    { "binopdf", (ptrBuiltin)Nelson::StatisticsGateway::binopdfBuiltin, 1, 3 },
    { "binocdf", (ptrBuiltin)Nelson::StatisticsGateway::binocdfBuiltin, 1, 4 },
    { "binoinv", (ptrBuiltin)Nelson::StatisticsGateway::binoinvBuiltin, 1, 3 },
    { "binornd", (ptrBuiltin)Nelson::StatisticsGateway::binorndBuiltin, 1, -1 },
    { "binostat", (ptrBuiltin)Nelson::StatisticsGateway::binostatBuiltin, 2, 2 },
    { "binofit", (ptrBuiltin)Nelson::StatisticsGateway::binofitBuiltin, 2, 3 },
    { "binolike", (ptrBuiltin)Nelson::StatisticsGateway::binolikeBuiltin, 2, 3 },
    { "poisspdf", (ptrBuiltin)Nelson::StatisticsGateway::poisspdfBuiltin, 1, 2 },
    { "poisscdf", (ptrBuiltin)Nelson::StatisticsGateway::poisscdfBuiltin, 1, 3 },
    { "poissinv", (ptrBuiltin)Nelson::StatisticsGateway::poissinvBuiltin, 1, 2 },
    { "poissrnd", (ptrBuiltin)Nelson::StatisticsGateway::poissrndBuiltin, 1, -1 },
    { "poissstat", (ptrBuiltin)Nelson::StatisticsGateway::poissstatBuiltin, 2, 1 },
    { "poissfit", (ptrBuiltin)Nelson::StatisticsGateway::poissfitBuiltin, 2, 2 },
    { "poisslike", (ptrBuiltin)Nelson::StatisticsGateway::poisslikeBuiltin, 2, 2 },
    { "nbinpdf", (ptrBuiltin)Nelson::StatisticsGateway::nbinpdfBuiltin, 1, 3 },
    { "nbincdf", (ptrBuiltin)Nelson::StatisticsGateway::nbincdfBuiltin, 1, 4 },
    { "nbininv", (ptrBuiltin)Nelson::StatisticsGateway::nbininvBuiltin, 1, 3 },
    { "nbinrnd", (ptrBuiltin)Nelson::StatisticsGateway::nbinrndBuiltin, 1, -1 },
    { "nbinstat", (ptrBuiltin)Nelson::StatisticsGateway::nbinstatBuiltin, 2, 2 },
    { "nbinfit", (ptrBuiltin)Nelson::StatisticsGateway::nbinfitBuiltin, 2, 5 },
    { "nbinlike", (ptrBuiltin)Nelson::StatisticsGateway::nbinlikeBuiltin, 2, 4 },
    { "unifpdf", (ptrBuiltin)Nelson::StatisticsGateway::unifpdfBuiltin, 1, 3 },
    { "unifcdf", (ptrBuiltin)Nelson::StatisticsGateway::unifcdfBuiltin, 1, 4 },
    { "unifinv", (ptrBuiltin)Nelson::StatisticsGateway::unifinvBuiltin, 1, 3 },
    { "unifit", (ptrBuiltin)Nelson::StatisticsGateway::unifitBuiltin, 4, 2 },
    { "uniflike", (ptrBuiltin)Nelson::StatisticsGateway::uniflikeBuiltin, 2, 4 },
    { "unifrnd", (ptrBuiltin)Nelson::StatisticsGateway::unifrndBuiltin, 1, -1 },
    { "unifstat", (ptrBuiltin)Nelson::StatisticsGateway::unifstatBuiltin, 2, 2 },
    { "unidpdf", (ptrBuiltin)Nelson::StatisticsGateway::unidpdfBuiltin, 1, 2 },
    { "unidcdf", (ptrBuiltin)Nelson::StatisticsGateway::unidcdfBuiltin, 1, 2 },
    { "unidinv", (ptrBuiltin)Nelson::StatisticsGateway::unidinvBuiltin, 1, 2 },
    { "unidrnd", (ptrBuiltin)Nelson::StatisticsGateway::unidrndBuiltin, 1, -1 },
    { "unidstat", (ptrBuiltin)Nelson::StatisticsGateway::unidstatBuiltin, 2, 1 },
    { "unidfit", (ptrBuiltin)Nelson::StatisticsGateway::unidfitBuiltin, 2, 2 },
    { "unidlike", (ptrBuiltin)Nelson::StatisticsGateway::unidlikeBuiltin, 2, 2 },
    { "chi2pdf", (ptrBuiltin)Nelson::StatisticsGateway::chi2pdfBuiltin, 1, 2 },
    { "chi2cdf", (ptrBuiltin)Nelson::StatisticsGateway::chi2cdfBuiltin, 1, 3 },
    { "chi2inv", (ptrBuiltin)Nelson::StatisticsGateway::chi2invBuiltin, 1, 2 },
    { "chi2rnd", (ptrBuiltin)Nelson::StatisticsGateway::chi2rndBuiltin, 1, -1 },
    { "chi2stat", (ptrBuiltin)Nelson::StatisticsGateway::chi2statBuiltin, 2, 1 },
    { "tpdf", (ptrBuiltin)Nelson::StatisticsGateway::tpdfBuiltin, 1, 2 },
    { "tcdf", (ptrBuiltin)Nelson::StatisticsGateway::tcdfBuiltin, 1, 3 },
    { "tinv", (ptrBuiltin)Nelson::StatisticsGateway::tinvBuiltin, 1, 2 },
    { "trnd", (ptrBuiltin)Nelson::StatisticsGateway::trndBuiltin, 1, -1 },
    { "tstat", (ptrBuiltin)Nelson::StatisticsGateway::tstatBuiltin, 2, 1 },
    { "fpdf", (ptrBuiltin)Nelson::StatisticsGateway::fpdfBuiltin, 1, 3 },
    { "fcdf", (ptrBuiltin)Nelson::StatisticsGateway::fcdfBuiltin, 1, 4 },
    { "finv", (ptrBuiltin)Nelson::StatisticsGateway::finvBuiltin, 1, 3 },
    { "frnd", (ptrBuiltin)Nelson::StatisticsGateway::frndBuiltin, 1, -1 },
    { "fstat", (ptrBuiltin)Nelson::StatisticsGateway::fstatBuiltin, 2, 2 },
    { "gampdf", (ptrBuiltin)Nelson::StatisticsGateway::gampdfBuiltin, 1, 3 },
    { "gamcdf", (ptrBuiltin)Nelson::StatisticsGateway::gamcdfBuiltin, 1, 4 },
    { "gaminv", (ptrBuiltin)Nelson::StatisticsGateway::gaminvBuiltin, 1, 3 },
    { "gamrnd", (ptrBuiltin)Nelson::StatisticsGateway::gamrndBuiltin, 1, -1 },
    { "gamstat", (ptrBuiltin)Nelson::StatisticsGateway::gamstatBuiltin, 2, 2 },
    { "gamfit", (ptrBuiltin)Nelson::StatisticsGateway::gamfitBuiltin, 2, 5 },
    { "gamlike", (ptrBuiltin)Nelson::StatisticsGateway::gamlikeBuiltin, 2, 4 },
    { "exppdf", (ptrBuiltin)Nelson::StatisticsGateway::exppdfBuiltin, 1, 2 },
    { "expcdf", (ptrBuiltin)Nelson::StatisticsGateway::expcdfBuiltin, 1, 3 },
    { "expinv", (ptrBuiltin)Nelson::StatisticsGateway::expinvBuiltin, 1, 2 },
    { "exprnd", (ptrBuiltin)Nelson::StatisticsGateway::exprndBuiltin, 1, -1 },
    { "expstat", (ptrBuiltin)Nelson::StatisticsGateway::expstatBuiltin, 2, 1 },
    { "expfit", (ptrBuiltin)Nelson::StatisticsGateway::expfitBuiltin, 2, 5 },
    { "explike", (ptrBuiltin)Nelson::StatisticsGateway::explikeBuiltin, 2, 4 },
    { "ttest", (ptrBuiltin)Nelson::StatisticsGateway::ttestBuiltin, 4, -1 },
    { "ttest2", (ptrBuiltin)Nelson::StatisticsGateway::ttest2Builtin, 4, -1 },
    { "ztest", (ptrBuiltin)Nelson::StatisticsGateway::ztestBuiltin, 4, -1 },
    { "vartest", (ptrBuiltin)Nelson::StatisticsGateway::vartestBuiltin, 4, -1 },
    { "vartest2", (ptrBuiltin)Nelson::StatisticsGateway::vartest2Builtin, 4, -1 },
    { "kstest", (ptrBuiltin)Nelson::StatisticsGateway::kstestBuiltin, 4, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "kstest2", (ptrBuiltin)Nelson::StatisticsGateway::kstest2Builtin, 3, -1 },
    { "linkage", (ptrBuiltin)Nelson::StatisticsGateway::linkageBuiltin, 1, 4 },
    { "cluster", (ptrBuiltin)Nelson::StatisticsGateway::clusterBuiltin, 1, -1 },
    { "pdist", (ptrBuiltin)Nelson::StatisticsGateway::pdistBuiltin, 1, 3 },
    { "pdist2", (ptrBuiltin)Nelson::StatisticsGateway::pdist2Builtin, 2, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "squareform", (ptrBuiltin)Nelson::StatisticsGateway::squareformBuiltin, 1, 2 },
    { "knnsearch", (ptrBuiltin)Nelson::StatisticsGateway::knnsearchBuiltin, 2, -1 },
    { "rangesearch", (ptrBuiltin)Nelson::StatisticsGateway::rangesearchBuiltin, 2, -1 },
    { "zscore", (ptrBuiltin)Nelson::StatisticsGateway::zscoreBuiltin, 3, 3 },
    { "quantile", (ptrBuiltin)Nelson::StatisticsGateway::quantileBuiltin, 1, -1 },
    { "prctile", (ptrBuiltin)Nelson::StatisticsGateway::prctileBuiltin, 1, -1 },
    { "iqr", (ptrBuiltin)Nelson::StatisticsGateway::iqrBuiltin, 2, -1 },
    { "dbscan", (ptrBuiltin)Nelson::StatisticsGateway::dbscanBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
    { "kmeans", (ptrBuiltin)Nelson::StatisticsGateway::kmeansBuiltin, -1, -1,
        CPP_BUILTIN_WITH_EVALUATOR },
};
//=============================================================================
NLS_GATEWAY_FUNC(gateway)
//=============================================================================
NLS_GATEWAY_REMOVE(gateway)
//=============================================================================
