%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = readtable(varargin)
    narginchk(1, 10);
    nargoutchk(0, 1);
    filename = convertStringsToChars(varargin{1});
    if (nargin > 1) && isa(varargin{2}, 'DelimitedTextImportOptions')
        options = varargin{2};
    else
        options = detectImportOptions(filename);
    end
    ce = readcell(filename, options);
    variableNames = options.VariableNames;
    VariableNamesLine = options.VariableNamesLine;
    RowNamesColumn = options.RowNamesColumn;
    if ~isempty(variableNames) && (VariableNamesLine > 0)
        ce(VariableNamesLine, :) = [];
    end
    if RowNamesColumn > 0
        variableNames(VariableNamesLine) = [];
        rowNames = ce(:, RowNamesColumn);
        ce(:, RowNamesColumn) = [];
    else
        rowNames = {};
    end
    args = {};
    if ~isempty(variableNames)
        args = [args, 'VariableNames', {variableNames}];
    end 
    if ~isempty(rowNames)
      args = [args, 'RowNames', {rowNames'}];
    end
    if isrow(ce)
        varargout{1} = table(ce{:}, args{:});
    else
        varargout{1} = table(ce, args{:});
    end
end
%=============================================================================
