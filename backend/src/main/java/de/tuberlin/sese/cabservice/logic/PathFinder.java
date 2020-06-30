package de.tuberlin.sese.cabservice.logic;

import de.tuberlin.sese.cabservice.persistence.cab.blocked.CabBlockedService;
import de.tuberlin.sese.cabservice.util.exceptions.NoPathException;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import static de.tuberlin.sese.cabservice.logic.Option.Direction.*;

@Builder
@Getter
@Setter
@Component
public class PathFinder {

    public PathFinder(CabBlockedService blockedService) {
        this.blockedService = blockedService;
    }

    private final CabBlockedService blockedService;

    public List<Option> getRouteBetween(int start, int end) {

        List<Option> route = new LinkedList<>();

        int currentSection = start;
        while (currentSection != end) {
            Optional<Option> nextActionOptional = getNextAction(currentSection, end);
            if (nextActionOptional.isPresent()) {
                Option nextAction = nextActionOptional.get();
                route.add(nextAction);
                if (WAIT.equals(nextAction.getDirection())) {
                    return route;
                }
                currentSection = nextAction.getToSection();
            } else {
                route.add(Option.builder()
                        .fromSection(currentSection)
                        .toSection(currentSection)
                        .direction(WAIT)
                        .build());
                return route;
            }
        }

        return route;
    }

    private Optional<Option> getNextAction(int currentSection, int endSection) {
        List<Integer> blockedSections = blockedService.getBlockedSections();

        Option onlyOption;
        Option primaryOption;
        Option alternativeOption;

        try {
            switch (currentSection) {
                case 0:
                    onlyOption = Option.builder()
                            .fromSection(0)
                            .toSection(13)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 1:
                    primaryOption = Option.builder()
                            .fromSection(1)
                            .toSection(3)
                            .direction(LEFT)
                            .build();
                    alternativeOption = Option.builder()
                            .fromSection(1)
                            .toSection(2)
                            .direction(RIGHT)
                            .build();

                    return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
                case 2:
                    onlyOption = Option.builder()
                            .fromSection(2)
                            .toSection(4)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 3:
                    onlyOption = Option.builder()
                            .fromSection(3)
                            .toSection(4)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 4:
                    primaryOption = Option.builder()
                            .fromSection(4)
                            .toSection(6)
                            .direction(LEFT)
                            .build();
                    alternativeOption = Option.builder()
                            .fromSection(4)
                            .toSection(5)
                            .direction(RIGHT)
                            .build();

                    return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
                case 5:
                    onlyOption = Option.builder()
                            .fromSection(5)
                            .toSection(7)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 6:
                    onlyOption = Option.builder()
                            .fromSection(6)
                            .toSection(7)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 7:
                    primaryOption = Option.builder()
                            .fromSection(7)
                            .toSection(9)
                            .direction(LEFT)
                            .build();
                    alternativeOption = Option.builder()
                            .fromSection(7)
                            .toSection(8)
                            .direction(RIGHT)
                            .build();

                    return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
                case 8:
                    onlyOption = Option.builder()
                            .fromSection(8)
                            .toSection(10)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 9:
                    onlyOption = Option.builder()
                            .fromSection(9)
                            .toSection(10)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 10:
                    primaryOption = Option.builder()
                            .fromSection(10)
                            .toSection(12)
                            .direction(LEFT)
                            .build();
                    alternativeOption = Option.builder()
                            .fromSection(10)
                            .toSection(11)
                            .direction(RIGHT)
                            .build();

                    return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
                case 11:
                    onlyOption = Option.builder()
                            .fromSection(11)
                            .toSection(1)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));
                case 12:
                    if (endSection == 14 || endSection == 15 || endSection == 0) {

                        if (blockedSections.contains(14)) {
                            throw new NoPathException();
                        }

                        return Optional.ofNullable(Option.builder()
                                .fromSection(12)
                                .toSection(14)
                                .direction(LEFT)
                                .build());
                    }

                    if (blockedSections.contains(13)) {
                        if (blockedSections.contains(14)) {
                            throw new NoPathException();
                        }

                        return Optional.ofNullable(Option.builder()
                                .fromSection(12)
                                .toSection(14)
                                .direction(LEFT)
                                .build());
                    }

                    return Optional.ofNullable(Option.builder()
                            .fromSection(12)
                            .toSection(13)
                            .direction(RIGHT)
                            .build());
                case 13:
                    onlyOption = Option.builder()
                            .fromSection(13)
                            .toSection(1)
                            .direction(STRAIGHT)
                            .build();

                    return Optional.of(handleChoice(onlyOption, blockedSections));

                case 14:
                    if (endSection == 15) {
                        return Optional.of(Option.builder()
                                .fromSection(14)
                                .toSection(15)
                                .direction(LEFT)
                                .build());
                    }
                    return Optional.of(Option.builder()
                            .fromSection(14)
                            .toSection(0)
                            .direction(RIGHT)
                            .build());
                case 15:
                    return Optional.empty();
                default:
                    throw new IllegalArgumentException("Unknown current section");
            }
        } catch (NoPathException e) {
            return Optional.of(Option.builder()
                    .fromSection(currentSection)
                    .toSection(currentSection)
                    .direction(WAIT)
                    .build());
        }
    }

    private Option handleChoice(Option primaryOption, Option alternativeOption, int endSection, List<Integer> blockedSections) throws NoPathException {
        if (endSection == alternativeOption.getToSection()) {
            if (blockedSections.contains(alternativeOption.getToSection())) {
                throw new NoPathException();
            }

            return alternativeOption;
        }

        if (blockedSections.contains(primaryOption.getToSection())) {
            if (blockedSections.contains(alternativeOption.getToSection())) {
                throw new NoPathException();
            }

            return alternativeOption;
        }

        return primaryOption;
    }

    private Option handleChoice(Option onlyOption, List<Integer> blockedSections) throws NoPathException {
        if (blockedSections.contains(onlyOption.getToSection())) {
            throw new NoPathException();
        }

        return onlyOption;
    }
}
