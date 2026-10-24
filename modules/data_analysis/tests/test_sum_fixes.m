%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(sum([1 NaN 3],'omitmissing'), 4);
asserts.isequal(sum([1 NaN 3],'includemissing'), NaN);
asserts.checkerror('sum(ones(2,2),[1 1])', _('Elements of a dimension vector must be unique.'));
asserts.isequal(sum(ones(2,3),'omitnan','double'), [2 2 2]);
asserts.isequal(sum(ones(2,3),'omitnan','native'), [2 2 2]);
asserts.isequal(sum(ones(2,3),2,'omitnan','double'), reshape([3 3], [2 1]));
asserts.checkerror('sum(ones(2,3,4),[2 2])', _('Elements of a dimension vector must be unique.'));
asserts.isequal(sum(sparse([1 2]),'omitmissing'), sparse(3));
%=============================================================================
