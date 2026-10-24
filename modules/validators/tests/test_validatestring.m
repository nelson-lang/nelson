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
asserts.isequal(nargin('validatestring'), -2);
asserts.isequal(nargout('validatestring'), 1);
%=============================================================================
R = validatestring('rect', {'square', 'rectangle', 'triangle'});
asserts.isequal(R, 'rectangle');
%=============================================================================
R = validatestring('ball', {'ball', 'balloon'});
asserts.isequal(R, 'ball');
%=============================================================================
R = validatestring('ballo', {'balloon', 'barn'});
asserts.isequal(R, 'balloon');
%=============================================================================
R = validatestring('TRI', {'square', 'rectangle', 'triangle'});
asserts.isequal(R, 'triangle');
%=============================================================================
R = validatestring("rig", ["righteously", "right", "righteous"]);
asserts.isequal(R, "right");
%=============================================================================
R = validatestring("right", ["left", "right"]);
asserts.isequal(R, "right");
%=============================================================================
R = validatestring("LEFT", {'left', 'right'});
asserts.isequal(R, 'left');
%=============================================================================
R = validatestring('a', 'abc');
asserts.isequal(R, 'abc');
%=============================================================================
assertErrorContains(@() validatestring('octagon', {'square', 'rectangle', 'triangle'}, 1), ...
  'did not match');
%=============================================================================
assertErrorContains(@() validatestring('ba', {'balloon', 'ballet'}), ...
  'matched more than one');
%=============================================================================
R = validatestring('r', {'right', 'righteous', 'righteously'});
asserts.isequal(R, 'right');
%=============================================================================
R = validatestring('m', {'mediumlong', 'medium', 'med'});
asserts.isequal(R, 'med');
%=============================================================================
assertErrorContains(@() validatestring('anything', {}), 'did not match');
%=============================================================================
assertErrorContains(@() validatestring(3, {'a'}), 'str');
assertErrorContains(@() validatestring('a', {1}), 'validStrings');
assertErrorContains(@() validatestring('a', {'a'}, 0), 'positive');
assertErrorIdentifier(@() validatestring('anything', {}), 'Nelson:validators:unrecognizedStringChoice');
assertErrorIdentifier(@() validatestring('ba', {'balloon', 'ballet'}), 'Nelson:validators:ambiguousStringChoice');
%=============================================================================
