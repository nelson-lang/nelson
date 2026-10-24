%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Encode labels as categorical data and count each category.
labels = categorical({'low'; 'high'; 'medium'; 'low'; 'high'; 'low'});
disp('Categories:');
disp(categories(labels));
disp('Counts:');
disp(countcats(labels));
