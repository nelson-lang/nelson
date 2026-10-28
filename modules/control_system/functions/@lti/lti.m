%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% lti is the shared base class for the tf, ss and zpk model classes.
% It only hosts internal helpers; it is never instantiated directly.
classdef lti
  methods (Hidden)
    sys = applyCommonMetadata(sys, left, right)
    sys = applySeriesMetadata(sys, left, right)
    displayModelProperties(sys, plainFallback)
    variable = mergeTfVariable(left, right, fallback)
    timeUnit = mergeTimeUnit(left, right)
    userData = mergeUserData(left, right)
    value = subscriptGet(obj, name)
    obj = subscriptSet(obj, name, value)
  end

  methods (Static, Hidden)
    checkSampleTimeCompatibility(left, right)
    value = copyCellMetadata(value, count)
    out = copyModelMetadata(out, in)
    value = mergeCellMetadata(left, leftCount, right, rightCount, mode)
    props = propertyNames(className)
    Ts = resolveSampleTime(left, right)
    value = selectIOProperty(value, indices, dim)
    value = validateSampleTime(value)
    value = validateTextScalar(name, value)
    value = validateTimeUnit(value)
  end
end
%=============================================================================
