%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% XML help belongs to the currently loaded module, not the global JSON index.
initialLanguage = getlanguage();
moduleName = ['help_xml_', strrep(createGUID(), '-', '_')];
moduleRoot = tempname(tempdir());
functionsRoot = fullfile(moduleRoot, 'functions');
namespaceRoot = fullfile(functionsRoot, ['+', moduleName]);
guide = [moduleName, '_guide'];
api = [moduleName, '.answer'];
fallback = [moduleName, '.fallback'];
alias = [moduleName, ':alias&detail'];
languages = {'en_US', 'fr_FR'};
try
    mkdir(fullfile(moduleRoot, 'etc'));
    mkdir(namespaceRoot);
    pathLiteral = strrep(functionsRoot, '''', '''''');
    filewrite(fullfile(moduleRoot, 'etc', 'startup.m'), ['addpath(''', pathLiteral, ''');']);
    filewrite(fullfile(moduleRoot, 'etc', 'finish.m'), ['rmpath(''', pathLiteral, ''');']);
    filewrite(fullfile(namespaceRoot, 'answer.m'), ['function value=answer()', newline(), ...
        '% Only a short macro header.', newline(), 'value=42;', newline(), 'end']);
    filewrite(fullfile(namespaceRoot, 'fallback.m'), ['function value=fallback()', newline(), ...
        '% Preserved macro-header fallback.', newline(), 'value=1;', newline(), 'end']);
    for index = 1:numel(languages)
        directory = fullfile(moduleRoot, 'help', languages{index}, 'xml');
        mkdir(fullfile(directory, 'nested'));
        writePage(fullfile(directory, 'nested', 'tutorial_page.xml'), guide, alias, languages{index}, true);
        writePage(fullfile(directory, [moduleName, '_answer.xml']), api, '', languages{index}, false);
        filewrite(fullfile(directory, 'malformed.xml'), '<xmldoc><keyword>');
        writePage(fullfile(directory, 'help.xml'), 'help', '', languages{index}, false);
    end
    englishDirectory = fullfile(moduleRoot, 'help', 'en_US', 'xml');
    defaultOnly = [moduleName, '_default_only'];
    writePage(fullfile(englishDirectory, 'fallback_language.xml'), defaultOnly, '', 'en_US', false);
    assert_istrue(isempty(help(guide)));
    addmodule(moduleRoot, moduleName);
    for index = 1:numel(languages)
        setlanguage(languages{index});
        text = help(guide);
        assert_istrue(contains(text, ['Full documentation ', languages{index}]));
        assert_istrue(contains(text, 'third_sentence_marker'));
        if strcmp(languages{index}, 'fr_FR')
            assert_istrue(contains(text, 'français'));
        end
        assert_istrue(contains(text, 'literal %s'));
        assert_istrue(contains(text, 'example_must_not_execute'));
        assert_istrue(contains(text, ['first_example', newline(), 'second_example']));
        assert_isequal(help(alias), text);
        assert_isequal(help('tutorial_page'), text);
        printed = evalc('help(guide)');
        assert_istrue(contains(printed, 'literal %s'));
        assert_istrue(contains(printed, 'example_must_not_execute'));
        apiText = help(api);
        assert_istrue(contains(apiText, ['Full documentation ', languages{index}]));
        assert_istrue(contains(apiText, 'input_marker'));
        assert_istrue(contains(apiText, 'output_marker'));
        assert_istrue(contains(apiText, 'third_sentence_marker'));
        assert_isfalse(contains(apiText, 'Only a short macro header'));
        assert_istrue(contains(help(defaultOnly), 'Full documentation en_US'));
        % The shipped global index still wins over an external duplicate key.
        indexFile = fullfile(modulepath('help_tools'), 'help', ['nelson_help_', languages{index}, '.json']);
        if isfile(indexFile)
            assert_isfalse(contains(help('help'), 'Full documentation'));
            assert_istrue(contains(help(fallback), 'Preserved macro-header fallback'));
        end
    end
    % Re-read changed pages; no cache may outlive the XML or module registration.
    setlanguage('en_US');
    changed = fullfile(englishDirectory, 'nested', 'tutorial_page.xml');
    filewrite(changed, strrep(fileread(changed), 'third_sentence_marker', 'updated_page_marker'));
    assert_istrue(contains(help(guide), 'updated_page_marker'));
    removemodule(moduleName);
    assert_istrue(isfile(changed));
    assert_istrue(isempty(help(guide)));
    assert_istrue(isempty(help(alias)));
    assert_istrue(isempty(help(api)));
    addmodule(moduleRoot, moduleName);
    assert_istrue(contains(help(guide), 'updated_page_marker'));
    cleanup(moduleName, moduleRoot, initialLanguage);
catch exception
    cleanup(moduleName, moduleRoot, initialLanguage);
    rethrow(exception);
end
%=============================================================================
function writePage(filename, keyword, alias, language, includeExamples)
    localized = 'English text. ';
    if strcmp(language, 'fr_FR')
        localized = 'Résumé français avec élève. ';
    end
    aliasText = '';
    if ~isempty(alias)
        aliasText = ['<keyword_alias>', strrep(alias, '&', '&amp;'), '</keyword_alias>'];
    end
    exampleText = '';
    if includeExamples
        exampleText = ['<examples><example_item><example_item_type>nelson</example_item_type>', ...
            '<example_item_data><![CDATA[first_example', newline(), ...
            'second_example', newline(), 'error(''example_must_not_execute''); % literal %s', ...
            ']]></example_item_data></example_item></examples>'];
    end
    xml = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
        '<language>', language, '</language><keyword>', keyword, '</keyword>', aliasText, ...
        '<short_description>Full documentation ', language, '</short_description>', ...
        '<syntax><syntax_item>value = ', keyword, '()</syntax_item></syntax>', ...
        '<param_input><param_input_item><param_name>x</param_name>', ...
        '<param_description>input_marker</param_description></param_input_item></param_input>', ...
        '<param_output><param_output_item><param_name>value</param_name>', ...
        '<param_description>output_marker</param_description></param_output_item></param_output>', ...
        '<description><p>', localized, 'First sentence. Second sentence. ', ...
        'third_sentence_marker literal %s remains visible.</p></description>', exampleText, '</xmldoc>'];
    filewrite(filename, xml);
end
%=============================================================================
function cleanup(moduleName, moduleRoot, language)
    setlanguage(language);
    if ismodule(moduleName)
        removemodule(moduleName);
    end
    if isdir(moduleRoot)
        rmdir(moduleRoot, 's');
    end
end
