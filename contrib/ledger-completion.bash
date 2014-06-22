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
    local cur prev command options
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
    # Options are found in source code:
    # global.cc::lookup_option
    # report.cc::lookup_option
    # session.cc::lookup_option
    #
    options="--abbrev-len= --account-width= --account= --actual --actual-dates --add-budget --amount-data --amount-width= --amount= --anon --ansi --args-only --auto-match --aux-date --average --balance-format= --base --basis --begin= --bold-if= --budget --budget-format= --by-payee --cache= --change --check-payees --cleared --cleared-format= --collapse --collapse-if-zero --color --columns= --cost --count --csv-format= --current --daily --date-format= --date-width= --date= --datetime-format= --day-break --days-of-week --dc --debug= --decimal-comma --depth= --detail --deviation --display-amount= --display-total= --display= --dow --download --effective --empty --end= --equity --exact --exchange= --explicit --file= --first= --flat --force-color --force-pager --forecast-while= --forecast-years= --forecast= --format= --full-help --gain --generated --group-by= --group-title-format= --head= --help --help-calc --help-comm --help-disp --historical --immediate --init-file= --inject= --input-date-format= --invert --last= --leeway= --limit= --lot-dates --lot-notes --lot-prices --lot-tags --lots --lots-actual --market --master-account= --meta-width= --meta= --monthly --no-aliases --no-color --no-pager --no-rounding --no-titles --no-total --now= --only= --options --output= --pager= --payee-width= --payee= --pedantic --pending --percent --period-sort= --period= --permissive --pivot= --plot-amount-format= --plot-total-format= --prepend-format= --prepend-width= --price --price-db= --price-exp= --pricedb-format= --prices-format= --primary-date --quantity --quarterly --raw --real --recursive-aliases --register-format= --related --related-all --revalued --revalued-only --revalued-total= --rich-data --script= --seed= --sort-all= --sort-xacts= --sort= --start-of-week= --strict --subtotal --tail= --time-colon --time-report --total-data --total-width= --total= --trace= --truncate= --unbudgeted --uncleared --unrealized --unrealized-gains= --unrealized-losses= --unround --value --value-expr= --values --verbose --verify --verify-memory --version --weekly --wide --yearly"

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
        --@(cache|file|init-file|output|pager|price-db|script))
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
