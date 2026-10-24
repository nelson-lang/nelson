%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
addpath(modulepath('validators', 'tests'));
%=============================================================================
p = inputParser();
asserts.istrue(isa(p, 'inputParser'));
asserts.isfalse(p.CaseSensitive);
asserts.isfalse(p.KeepUnmatched);
asserts.istrue(p.PartialMatching);
asserts.istrue(p.StructExpand);
%=============================================================================
p.CaseSensitive = 1;
asserts.istrue(p.CaseSensitive);
p.CaseSensitive = false;
p.KeepUnmatched = 1;
asserts.istrue(p.KeepUnmatched);
p.KeepUnmatched = false;
p.PartialMatching = 0;
asserts.isfalse(p.PartialMatching);
p.PartialMatching = true;
p.StructExpand = 0;
asserts.isfalse(p.StructExpand);
p.StructExpand = true;
%=============================================================================
addRequired(p, 'width', @(x) isnumeric(x) && isscalar(x) && x > 0);
addOptional(p, 'height', 1, @(x) isnumeric(x) && isscalar(x) && x > 0);
addParameter(p, 'units', 'm', @(x) ischar(x) || (isstring(x) && isscalar(x)));
asserts.isequal(p.Parameters, {'width', 'height', 'units'});
parse(p, 7, 'un', 'cm');
asserts.isequal(p.Results.width, 7);
asserts.isequal(p.Results.height, 1);
asserts.isequal(p.Results.units, 'cm');
asserts.isequal(p.UsingDefaults, {'height'});
asserts.isequal(fieldnames(p.Unmatched), cell(0, 1));
%=============================================================================
parse(p, 7, 3, 'units', 'in');
asserts.isequal(p.Results.height, 3);
asserts.isequal(p.Results.units, 'in');
asserts.istrue(isempty(p.UsingDefaults));
%=============================================================================
p.FunctionName = 'areaParser';
asserts.isequal(p.FunctionName, 'areaParser');
%=============================================================================
p2 = inputParser();
p2.KeepUnmatched = true;
addParameter(p2, 'known', 0);
parse(p2, 'other', 5, 'known', 2);
asserts.isequal(p2.Results.known, 2);
asserts.isequal(p2.Unmatched.other, 5);
%=============================================================================
p2b = inputParser();
addParameter(p2b, 'level', 0);
parse(p2b, 'level', 1, 'level', 9);
asserts.isequal(p2b.Results.level, 9);
asserts.istrue(isempty(p2b.UsingDefaults));
%=============================================================================
p3 = inputParser();
p3.CaseSensitive = true;
addParameter(p3, 'InputName', 0);
assertErrorContains(@() parse(p3, 'inputname', 10), 'Unrecognized');
parse(p3, 'InputName', 10);
asserts.isequal(p3.Results.InputName, 10);
%=============================================================================
p4 = inputParser();
addParameter(p4, 'input1', 0);
addParameter(p4, 'input2', 0);
s.input1 = 10;
s.input2 = 20;
parse(p4, s);
asserts.isequal(p4.Results.input1, 10);
asserts.isequal(p4.Results.input2, 20);
%=============================================================================
p4b = inputParser();
p4b.KeepUnmatched = true;
addParameter(p4b, 'input1', 0);
addParameter(p4b, 'input2', 0);
s2.input1 = 11;
s2.extra = 12;
s2.input2 = 13;
parse(p4b, s2);
asserts.isequal(p4b.Results.input1, 11);
asserts.isequal(p4b.Results.input2, 13);
asserts.isequal(p4b.Unmatched.extra, 12);
asserts.istrue(isempty(p4b.UsingDefaults));
%=============================================================================
p5 = inputParser();
p5.StructExpand = false;
addRequired(p5, 'structInput');
parse(p5, s);
asserts.istrue(isstruct(p5.Results.structInput));
%=============================================================================
p6 = inputParser();
p6.KeepUnmatched = true;
addParameter(p6, 'alpha', 0);
addParameter(p6, 'alpine', 0);
parse(p6, 'al', 4);
asserts.isequal(p6.Unmatched.al, 4);
%=============================================================================
p6b = inputParser();
p6b.PartialMatching = false;
addParameter(p6b, 'alpha', 0);
assertErrorContains(@() parse(p6b, 'al', 4), 'Unrecognized');
parse(p6b, 'alpha', 4);
asserts.isequal(p6b.Results.alpha, 4);
%=============================================================================
p7 = inputParser();
addParamValue(p7, 'mode', 'fast');
parse(p7, 'mode', 'slow');
asserts.isequal(p7.Results.mode, 'slow');
%=============================================================================
assertErrorContains(@() addRequired(p7, 'mode'), 'unique');
assertErrorContains(@() addParameter(p7, 'MODE', 0), 'unique');
assertErrorContains(@() parse(p, -1), 'invalid');
assertErrorContains(@() parse(p, 1, 'units'), 'paired');
assertErrorContains(@() parse(p, 1, 'unknown', 2), 'Unrecognized');
assertErrorContains(@() assignPropertyForTest(p, 'Results', struct()), 'Results');
assertErrorContains(@() assignPropertyForTest(p, 'Parameters', {}), 'Parameters');
assertErrorContains(@() assignPropertyForTest(p, 'UsingDefaults', {}), 'UsingDefaults');
assertErrorContains(@() addParameter(p7, 'bad-name', 0), 'identifier');
assertErrorContains(@() assignPropertyForTest(p, 'FunctionName', 3), 'FunctionName');
assertErrorContains(@() assignPropertyForTest(p, 'CaseSensitive', [true false]), 'CaseSensitive');
assertErrorIdentifier(@() addRequired(p7, 'mode'), 'Nelson:validators:inputParserDuplicateName');
assertErrorIdentifier(@() addParameter(p7, 'bad-name', 0), 'Nelson:validators:inputParserInvalidName');
assertErrorIdentifier(@() parse(p, 1, 'units'), 'Nelson:validators:inputParser:nameValueNotPaired');
assertErrorIdentifier(@() parse(p, 1, 'unknown', 2), 'Nelson:validators:inputParser:unrecognizedParameterName');
%=============================================================================
p8 = inputParser();
addRequired(p8, 'name', @(x) validateattributes(x, {'char'}, {'nonempty'}));
addOptional(p8, 'id', 1, @(x) validateattributes(x, {'numeric'}, {'scalar', 'positive'}));
addParameter(p8, 'tag', 'none', @(x) validatestring(x, {'none', 'primary'}));
parse(p8, 'item', 3, 'tag', 'pri');
asserts.isequal(p8.Results.name, 'item');
asserts.isequal(p8.Results.id, 3);
asserts.isequal(p8.Results.tag, 'pri');
assertErrorContains(@() parse(p8, [], 3), 'invalid');
assertErrorIdentifier(@() parse(p8, [], 3), 'Nelson:validators:inputParserInvalidValue');
%=============================================================================
