# Все исходные файлы .erl в текущей директории
ERL_FILES = $(wildcard *.erl)

# Скомпилированные файлы .beam
BEAM_FILES = $(patsubst %.erl,%.beam,$(ERL_FILES))

# Команда компиляции Erlang
ERLC = erlc

# Опции компилятора
ERLC_OPTS =

# Задачи Makefile

# Задача по умолчанию: компиляция всех .erl файлов
all: $(BEAM_FILES)

# Правило для компиляции .erl файлов в .beam файлы
%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

# Очистка скомпилированных файлов
clean:
	rm -f *.beam

# Задача для очистки и пересборки проекта
rebuild: clean all

# Задача для тестирования с помощью EUnit
test: all
	erl -noshell -eval "eunit:test([$(MODULES)], [verbose]), init:stop()."

# Получение имен модулей из файлов .erl
MODULES := $(patsubst %.erl,%,$(ERL_FILES))

.PHONY: all clean rebuild test
