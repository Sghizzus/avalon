# Istruzioni per l'Agente

## Modifiche ai file

**REGOLA FONDAMENTALE**: Quando l'utente chiede una modifica a un file, devi:

1. **Applicare DIRETTAMENTE le modifiche al file** - Non scrivere mai il codice in chat aspettandoti che l'utente lo copi. Usa sempre lo strumento `positron_editFile_internal` per modificare i file.

2. **Verificare SEMPRE che le modifiche siano state applicate** - Dopo ogni modifica, leggi il file con `positron_getFileContents_internal` per confermare che il cambiamento sia effettivo.

3. **Essere CHIRURGICO nelle modifiche** - Non sovrascrivere parti di codice che non devono essere modificate. In particolare:
   - NON cancellare mai i `library()` all'inizio dei file R
   - NON cancellare mai i `source()` che caricano altri file
   - NON cancellare parti di codice non correlate alla modifica richiesta
   - Usare `# ...existing code...` per preservare il codice esistente

4. **Fare modifiche mirate** - Modificare SOLO le righe necessarie, non riscrivere intere sezioni di codice a meno che non sia esplicitamente richiesto.

## Errori comuni da evitare

- Proporre codice in chat invece di applicarlo al file
- Applicare modifiche che sovrascrivono l'inizio del file (librerie, source)
- Non verificare che le modifiche siano state effettivamente applicate
- Modificare righe sbagliate del file

## Workflow corretto

1. Leggere il file per capire la struttura attuale
2. Identificare le righe esatte da modificare
3. Applicare le modifiche con `positron_editFile_internal`
4. Leggere di nuovo il file per verificare che le modifiche siano state applicate correttamente
5. Se le modifiche non sono state applicate, riprovare