%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Parse a compact set of comma-separated measurement records.
records = ["temperature,21.5,C"; "pressure,101.3,kPa"; "humidity,48.0,%"];
fields = split(records, ',');
measurementNames = fields(:, 1);
measurementValues = str2double(fields(:, 2));
measurementUnits = fields(:, 3);
summaryLines = compose('%s: %.1f %s', measurementNames, measurementValues, measurementUnits);
disp(summaryLines);
