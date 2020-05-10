# Quality Assurance

## Code Quality

- code should be elegant, efficient, readable, simple, without duplications, and well-written
- meaningful names
  - names of the classes, variables, and methods must be meaningful and clearly indicate what a method does or what an attribute is for
  - avoid acronyms and avoid confusing names, which may bring anyone who reads the code to the wrong conclusions
- methods
  - easy to read and understand
  - small (up to 25 lines)
  - should only do one thing :arrow_right: easier testing
  - as few parameters as possible
- comments
  - don't explain what you're doing, explain why you're doing it
  - if you have to explain what you're doing, refactor the code
- formatting
  - all team members should use the same code formatter
- error handling
  - handle all possible errors
  - write tests for all possible error cases

## Testing

Legende:

| Symbol               | Bedeutung                  |
| -------------------- | -------------------------- |
| :heavy_check_mark:   | Ja                         |
| :large_blue_diamond: | Mit manueller Vorbereitung |
| :x:                  | Nein                       |

| Art              | System under test                     | Implementiert in ..           | Automatisiert        | In Pileline        | Beschreibung                                                 | Beispiel                                                     |
| ---------------- | ------------------------------------- | ----------------------------- | -------------------- | ------------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
| Unit Test        | Externer Controller                   | Ada code                      | :heavy_check_mark:   | :heavy_check_mark: | Tests einzelne Methoden und Funktionalitäten. Hat keine Abhängigkeiten außerhalb der Methode/Funktionalität. | Berechnet Methode x wirklich einen Mittelwert der 4 Parameter? |
| Integration Test | Externer Controller + Webots Umgebung | Ada code                      | :large_blue_diamond: | :x:                | Testet das Verhalten des externen Controllers im Zusammenspiel mit Webots | Szenario: Cab fährt auf Hindernis zu.<br />Test: Cab kommt rechtzeitig zum Stehen und verhindert eine Kollision |
| Integration Test | Externer Controller                   | Ada code                      | :heavy_check_mark:   | :heavy_check_mark: | Prüft, ob der externe Controller auf (z.B.) Anfragen an die REST API wie erwartet reagiert | Achknowledged der externe Controller einen Auftrag des Backends? |
| Unit Test        | Frontend Komponente                   | Backend (Frontend Komponente) | :heavy_check_mark:   | :heavy_check_mark: | Prüft, ob sich die Frontend-Komponente wie gewünscht verhält | Wird dem Benutzer bei Fehlern in der Kommunikation eine Fehlermeldung angezeigt? |
| Unit Test        | Backend                               | Backend                       | :heavy_check_mark:   | :heavy_check_mark: | Prüft, ob einzelne Methoden und Funktionalitäten des Backends wie gewünscht funktionieren | Wird eine Nachricht für einen externen Controller richtig serialisiert? |
| Integration Test | Backend                               | Backend                       | :heavy_check_mark:   | :heavy_check_mark: | Prüft, ob das Backend auf (z.B.) Anfragen an die REST API wie erwartet reagiert | Antwortet das Backend bei Anfrage der aktuellen Infos der Cabs wie erwartet? |
| System Test      | Komplettes System                     | Backend                       | :large_blue_diamond: | :x:                | Testet highlevel Funktionalität des Backends.                | Input: Fahre zu Station A, sammele alle Fahrgäste ein, fahre zu Station B, liefere alle Fahrgäste ab, fahre ins Depot |
| Acceptance Test  | Komplettes System                     | :x:                           | :x:                  | :x:                | Manuelles Testen von zuvor definierten Akzeptanzkriterien    |                                                              |

Zusätzlich wird eine statische Code-Analyse für den externen Controller sowie das Backend + Frontend-Komponente in die Pipeline integriert.

## Formal Verification, Analysis, Optimization

:x: TODO: Hier muss Chanki sich als Masterstudent um Planung und Umsetzung kümmern :x:

## Git

Es wird der Git Feature Branch Workflow umgesetzt.

- Es wird durch GitLab Einstellungen verboten, direkt auf den master zu pushen
- Es wird durch Absprache verboten, eigene Commits in den master zu mergen
- Jede Änderung wird auf einem Feature-Branch entwickelt
- Für alle Änderungen, die im master landen sollen, muss eine Merge Request erstellt werden
- Ein Teammitglied, welches nicht an dem Feature gearbeitet hat, muss die Änderungen prüfen, Anmerkungen machen, und ggf. mergen