# Имя каталога с скомпилированными файлами
EBIN_DIR = ebin

# Все исходные файлы .erl в текущей директории
ERL_FILES = $(wildcard *.erl)

# Скомпилированные файлы .beam
BEAM_FILES = $(patsubst %.erl,$(EBIN_DIR)/%.beam,$(ERL_FILES))

# Команда компиляции Erlang
ERLC = erlc

# Опции компилятора
ERLC_OPTS = -o $(EBIN_DIR)

# Задачи Makefile

# Задача по умолчанию: компиляция всех .erl файлов
all: $(EBIN_DIR) $(BEAM_FILES)

# Создание каталога для скомпилированных файлов
$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

# Правило для компиляции .erl файлов в .beam файлы
$(EBIN_DIR)/%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

# Очистка скомпилированных файлов
clean:
	rm -rf $(EBIN_DIR)

# Задача для очистки и пересборки проекта
rebuild: clean all

.PHONY: all clean rebuild
