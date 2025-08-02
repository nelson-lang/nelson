%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% write a word document
%==============================================================================
pWord = actxserver('Word.Application')
pWord.Visible = true
pDocs = pWord.Documents
p = invoke( pDocs, 'Add')
pSelection = invoke(pWord, 'Selection')
pSelection.Text = 'Hello text in Word'
pActiveDocument = pWord.ActiveDocument
invoke(pActiveDocument, 'SaveAs', [tempdir(), 'foo.doc'])
invoke(pWord, 'Quit')
delete(pWord)
clear pWord pDocs pSelection pActiveDocument
%==============================================================================
% read Word document
%==============================================================================
pWord = actxserver('Word.Application')
pWord.Visible = true
pDocuments = get(pWord, 'Documents')
invoke(pDocuments, 'Open', [tempdir(), 'foo.doc']);
pActiveDocument = get(pWord, 'ActiveDocument');
pContent  = invoke(pActiveDocument,'Content');
pText = invoke(pContent,'Text');
disp(pText)
invoke(pWord, 'Quit');
delete(pWord)
clear pWord pDocuments pActiveDocument pContent pText
%==============================================================================
