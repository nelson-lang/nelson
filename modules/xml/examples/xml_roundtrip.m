%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Parse XML text, write a document and read it back.
filename = [tempdir(), 'nelson-example.xml'];
xmlText = '<measurements><value unit="C">21.4</value></measurements>';
xmlwrite(filename, xmlText);
document = xmlread(filename);
disp(xmlwrite(document));
