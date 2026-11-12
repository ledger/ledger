### Assumption
#
# bash-completion package is installed and enabled
#
### Just want to try it?
#
# $ source ledger-completion.bash
#
### How to install?
#
#### For local user
#
# $ cat <<EOF >>~/.bash_completion
# . ~/.bash_completion.d/ledger
# EOF
#
# $ cp ledger-completion.bash ~/.bash_completion.d/ledger
#
#### For all users
#
# $ sudo cp ledger-completion.bash /etc/bash_completion.d/ledger
#

_ledger()
{
    local cur prev commands options
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # COMMANDS
    #
    # Commands are found in source code:
    # report.cc::lookup "case symbol_t::COMMAND"
    # report.cc::lookupcase "case symbol_t::PRECOMMAND" : these are debug commands and they have been filtered out here
    #
    commands="accounts balance budget cleared commodities convert csv draft echo emacs entry equity lisp org payees pricemap prices pricesdb print register reload select source stats tags xact xml"

    # OPTIONS
    #
    # Every option a Debug build lists, plus the alternate names that
    # OPT_ALT gives in src/session.cc and src/report.cc.  An option that
    # takes an argument ends in "=".  Regenerate the list with
    #   ledger --args-only --debug option.names parse true 2>&1 >/dev/null |
    #     sed -n 's/.*\[DEBUG\] *Option: *//p' |
    #     sed -E 's/_$/=/; s/_/-/g; s/^/--/'
    # merged with the OPT_ALT names and sorted with LC_ALL=C sort -u.
    # test/CheckCompletion.py fails when this list drifts from ledger.
    #
    options="--abbrev-len= --account-rewrite= --account-value-expr= --account-width= --account= --actual --actual-dates --add-budget --align-intervals --amount-data --amount-width= --amount= --anon --ansi --append-format= --args-only --auto-match --aux-date --average --average-lot-prices --balance-format= --base --basis --begin= --bold-if= --both-directions --budget --budget-format= --by-payee --change --check-in-file-order --check-payees --cleared --cleared-format= --collapse --collapse-if-zero --collapse-if= --color --colors= --columns= --commodity-value-expr= --cost --count --csv-format= --csv-separator= --current --daily --date-format= --date-width= --date= --datetime-format= --day-break --days-of-week --dc --debug= --decimal-comma --decimal-places= --depth= --detail --deviation --display-account= --display-amount= --display-total= --display= --dow --download --effective --empty --end= --equity --exact --exchange= --explicit --file= --first= --flat --force-color --force-pager --forecast-while= --forecast-years= --forecast= --format= --gain --gain-since= --generated --getquote= --group-by-cumulative --group-by= --group-title-format= --hashes= --head= --help --historical --ignore-diacritics --immediate --import= --init-file= --inject= --input-date-format= --invert --last= --latest --leeway= --limit= --lisp-date-format= --lot-dates --lot-matching= --lot-notes --lot-prices --lot-tags --lots --lots-actual --lots-fifo --lots-lifo --market --master-account= --meta-width= --meta= --monthly --no-aliases --no-color --no-group-by --no-pager --no-revalued --no-rounding --no-titles --no-total --now= --only= --options --output= --pager= --payee-rewrite= --payee-width= --payee= --pedantic --pedantic-commodity --pending --percent --period-shift= --period-sort= --period= --permissive --pivot-only= --pivot= --plopen --plot-amount-format= --plot-total-format= --prepend-format= --prepend-width= --price --price-db= --price-exp= --pricedb-format= --prices-format= --primary-date --quantity --quarterly --raw --real --recursive-aliases --register-format= --related --related-all --revalued --revalued-only --revalued-total= --rich-data --round --script= --seed= --sort-all= --sort-xacts= --sort= --start-of-week= --strict --strict-commodity --subtotal --tail= --time-colon --time-report --time-round= --total-data --total-width= --total= --trace= --truncate= --unbudgeted --uncleared --unrealized --unrealized-gains= --unrealized-losses= --unround --value --value-expr= --values --verbose --verify --verify-memory --version --weekly --wide --yearly"

    # Bash FAQ E13 http://tiswww.case.edu/php/chet/bash/FAQ
    #
    COMP_WORDBREAKS=${COMP_WORDBREAKS//:}

    # ACCOUNTS
    #
    # Accounts are generated with bash command:
    # $ ledger accounts>/tmp/accounts; for i in {1..5}; do cut -d : -f $i- /tmp/accounts;cut -d : -f -$i /tmp/accounts; done|sort -u|xargs
    #
    # Warning: this is working badly if there are spaces in account names
    #
    accounts="Assets Liabilities Equity Revenue Expenses"

    case $prev in
        --@(file|init-file|output|pager|price-db|script)|-@(f|i|o))
            _filedir
            return 0
            ;;
        @(balance|equity|print|register))
            COMPREPLY=( $(compgen -W "${accounts}" -- ${cur}) )
            return 0
            ;;

    esac

    if [[ ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${options}" -- ${cur}) )
#    elif [[ ${cur} == [A-Z]* ]] ; then
#        COMPREPLY=( $(compgen -W "${accounts}" -- ${cur}) )
    else
        COMPREPLY=( $(compgen -W "${commands}" -- ${cur}) )
    fi

    return 0
}
complete -F _ledger ledger

# Local variables:
# mode: shell-script
# sh-basic-offset: 4
# sh-indent-comment: t
# indent-tabs-mode: nil
# End:
# ex: ts=4 sw=4 et filetype=sh
