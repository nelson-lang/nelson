//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "nlsStatistics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct NormalBounds
{
    ArrayOf value;
    ArrayOf lower;
    ArrayOf upper;
};
//=============================================================================
struct TTestResult
{
    ArrayOf h;
    ArrayOf p;
    ArrayOf ci;
    ArrayOf stats;
};
//=============================================================================
struct KSTestResult
{
    ArrayOf h;
    ArrayOf p;
    ArrayOf statistic;
    ArrayOf criticalValue;
};
//=============================================================================
struct KSTest2Result
{
    ArrayOf h;
    ArrayOf p;
    ArrayOf statistic;
};
//=============================================================================
struct UniformStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct UniformFitResult
{
    ArrayOf aHat;
    ArrayOf bHat;
    ArrayOf aCI;
    ArrayOf bCI;
};
//=============================================================================
struct UniformLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct DiscreteUniformFitResult
{
    ArrayOf nHat;
    ArrayOf nCI;
};
//=============================================================================
struct DiscreteUniformLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct LognormalStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct LognormalFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct LognormalLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct RayleighStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct RayleighFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct RayleighLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct ExtremeValueStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct ExtremeValueFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct ExtremeValueLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct GeneralizedExtremeValueStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct GeneralizedExtremeValueFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct GeneralizedExtremeValueLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct WeibullStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct WeibullFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct WeibullLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct GeometricStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct GeometricFitResult
{
    ArrayOf pHat;
    ArrayOf pCI;
};
//=============================================================================
struct GeometricLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct ExpStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct ExpFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct ExpLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct NormalStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct NormalFitResult
{
    ArrayOf muHat;
    ArrayOf sigmaHat;
    ArrayOf muCI;
    ArrayOf sigmaCI;
};
//=============================================================================
struct NormalLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct GammaStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct GammaFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct GammaLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct ChiSquareStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct TStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct FStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct BetaStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct BetaFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct BetaLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct BinomialStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct BinomialFitResult
{
    ArrayOf pHat;
    ArrayOf pCI;
};
//=============================================================================
struct BinomialLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct PoissonStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct PoissonFitResult
{
    ArrayOf lambdaHat;
    ArrayOf lambdaCI;
};
//=============================================================================
struct PoissonLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
struct NegativeBinomialStatResult
{
    ArrayOf mean;
    ArrayOf variance;
};
//=============================================================================
struct NegativeBinomialFitResult
{
    ArrayOf parameterHat;
    ArrayOf parameterCI;
};
//=============================================================================
struct NegativeBinomialLikeResult
{
    ArrayOf negativeLogLikelihood;
    ArrayOf covariance;
};
//=============================================================================
NLSSTATISTICS_IMPEXP NormalBounds
NormalCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NormalBounds
NormalInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
NormalRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NormalStatResult
NormalStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NormalFitResult
NormalFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NormalLikeResult
NormalLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
Chi2Pdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
Chi2Cdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
Chi2Inv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ChiSquareStatResult
ChiSquareStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ChiSquareRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
TPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
TCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
TInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP TStatResult
TStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
TRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
FPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
FCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
FInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP FStatResult
FStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
FRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GammaPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GammaCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GammaInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GammaRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GammaStatResult
GammaStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GammaFitResult
GammaFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GammaLikeResult
GammaLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExpPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExpCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExpInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExpRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GeometricFitResult
GeometricFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GeometricLikeResult
GeometricLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ExpStatResult
ExpStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ExpFitResult
ExpFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ExpLikeResult
ExpLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BetaPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BetaCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BetaInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BetaRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP BetaStatResult
BetaStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP BetaFitResult
BetaFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP BetaLikeResult
BetaLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BinomialPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BinomialCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BinomialInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
BinomialRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP BinomialStatResult
BinomialStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP BinomialFitResult
BinomialFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP BinomialLikeResult
BinomialLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
PoissonPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
PoissonCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
PoissonInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
PoissonRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP PoissonStatResult
PoissonStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP PoissonFitResult
PoissonFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP PoissonLikeResult
PoissonLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
NegativeBinomialPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
NegativeBinomialCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
NegativeBinomialInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
NegativeBinomialRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NegativeBinomialStatResult
NegativeBinomialStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NegativeBinomialFitResult
NegativeBinomialFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NegativeBinomialLikeResult
NegativeBinomialLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
UniformPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
UniformCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
UniformInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
UniformRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP UniformStatResult
UniformStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP UniformFitResult
UniformFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP UniformLikeResult
UniformLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
DiscreteUniformPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
DiscreteUniformCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
DiscreteUniformInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
DiscreteUniformRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP UniformStatResult
DiscreteUniformStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP DiscreteUniformFitResult
DiscreteUniformFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP DiscreteUniformLikeResult
DiscreteUniformLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
LognormalPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NormalBounds
LognormalCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP NormalBounds
LognormalInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
LognormalRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP LognormalStatResult
LognormalStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP LognormalFitResult
LognormalFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP LognormalLikeResult
LognormalLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
RayleighPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
RayleighCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
RayleighInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
RayleighRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP RayleighStatResult
RayleighStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP RayleighFitResult
RayleighFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP RayleighLikeResult
RayleighLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExtremeValuePdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExtremeValueCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExtremeValueInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
ExtremeValueRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ExtremeValueStatResult
ExtremeValueStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ExtremeValueFitResult
ExtremeValueFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ExtremeValueLikeResult
ExtremeValueLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeneralizedExtremeValuePdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeneralizedExtremeValueCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeneralizedExtremeValueInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeneralizedExtremeValueRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GeneralizedExtremeValueStatResult
GeneralizedExtremeValueStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GeneralizedExtremeValueFitResult
GeneralizedExtremeValueFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GeneralizedExtremeValueLikeResult
GeneralizedExtremeValueLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
WeibullPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
WeibullCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
WeibullInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
WeibullRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP WeibullStatResult
WeibullStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP WeibullFitResult
WeibullFit(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP WeibullLikeResult
WeibullLike(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeometricPdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeometricCdf(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeometricInv(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
GeometricRnd(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP GeometricStatResult
GeometricStat(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP TTestResult
TTest(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP TTestResult
TTest2(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP TTestResult
ZTest(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP TTestResult
VarTest(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP TTestResult
VarTest2(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP KSTestResult
KSTest(Evaluator* eval, const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP KSTest2Result
KSTest2(const ArrayOfVector& args);
//=============================================================================
NLSSTATISTICS_IMPEXP double
StudentTCdf(double t, double nu);
//=============================================================================
NLSSTATISTICS_IMPEXP double
StudentTInv(double p, double nu);
//=============================================================================
NLSSTATISTICS_IMPEXP double
NormalCdf01(double z);
//=============================================================================
NLSSTATISTICS_IMPEXP double
NormalInv01(double p);
//=============================================================================
NLSSTATISTICS_IMPEXP double
ChiSquareCdf(double x, double nu);
//=============================================================================
NLSSTATISTICS_IMPEXP double
ChiSquareUpper(double x, double nu);
//=============================================================================
NLSSTATISTICS_IMPEXP double
ChiSquareInv(double p, double nu);
//=============================================================================
NLSSTATISTICS_IMPEXP double
FCdf(double x, double df1, double df2);
//=============================================================================
NLSSTATISTICS_IMPEXP double
FUpper(double x, double df1, double df2);
//=============================================================================
NLSSTATISTICS_IMPEXP double
FInv(double p, double df1, double df2);
//=============================================================================
} // namespace Nelson
//=============================================================================
