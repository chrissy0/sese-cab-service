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

    public Optional<List<Option>> getRouteBetween(int start, int end) throws NoPathException {

        List<Option> route = new LinkedList<>();

        int currentSection = start;
        while (currentSection != end) {
            Optional<Option> nextActionOptional = getNextAction(currentSection, end);
            if (nextActionOptional.isPresent()) {
                Option nextAction = nextActionOptional.get();
                route.add(nextAction);
                currentSection = nextAction.getSection();
            } else {
                return Optional.empty();
            }
        }

        return Optional.of(route);
    }

    private Optional<Option> getNextAction(int currentSection, int endSection) throws NoPathException {
        List<Integer> blockedSections = blockedService.getBlockedSections();

        Option onlyOption;
        Option primaryOption;
        Option alternativeOption;
        switch (currentSection) {
            case 0:
                onlyOption = Option.builder()
                        .section(13)
                        .direction(STRAIGHT)
                        .build();

                return Optional.of(handleChoice(onlyOption, blockedSections));
            case 1:
                primaryOption = Option.builder()
                        .section(3)
                        .direction(LEFT)
                        .build();
                alternativeOption = Option.builder()
                        .section(2)
                        .direction(RIGHT)
                        .build();

                return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
            case 2:
            case 3:
                onlyOption = Option.builder()
                        .section(4)
                        .direction(STRAIGHT)
                        .build();

                return Optional.of(handleChoice(onlyOption, blockedSections));
            case 4:
                primaryOption = Option.builder()
                        .section(6)
                        .direction(LEFT)
                        .build();
                alternativeOption = Option.builder()
                        .section(5)
                        .direction(RIGHT)
                        .build();

                return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
            case 5:
            case 6:
                onlyOption = Option.builder()
                        .section(7)
                        .direction(STRAIGHT)
                        .build();

                return Optional.of(handleChoice(onlyOption, blockedSections));
            case 7:
                primaryOption = Option.builder()
                        .section(9)
                        .direction(LEFT)
                        .build();
                alternativeOption = Option.builder()
                        .section(8)
                        .direction(RIGHT)
                        .build();

                return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
            case 8:
            case 9:
                onlyOption = Option.builder()
                        .section(10)
                        .direction(STRAIGHT)
                        .build();

                return Optional.of(handleChoice(onlyOption, blockedSections));
            case 10:
                primaryOption = Option.builder()
                        .section(12)
                        .direction(LEFT)
                        .build();
                alternativeOption = Option.builder()
                        .section(11)
                        .direction(RIGHT)
                        .build();

                return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
            case 11:
            case 13:
                onlyOption = Option.builder()
                        .section(1)
                        .direction(STRAIGHT)
                        .build();

                return Optional.of(handleChoice(onlyOption, blockedSections));
            case 12:
                if (endSection == 14 || endSection == 15 || endSection == 0) {

                    if (blockedSections.contains(14)) {
                        throw new NoPathException();
                    }

                    return Optional.ofNullable(Option.builder()
                            .section(14)
                            .direction(LEFT)
                            .build());
                }

                if (blockedSections.contains(13)) {
                    if (blockedSections.contains(14)) {
                        throw new NoPathException();
                    }

                    return Optional.ofNullable(Option.builder()
                            .section(14)
                            .direction(LEFT)
                            .build());
                }

                return Optional.ofNullable(Option.builder()
                        .section(13)
                        .direction(RIGHT)
                        .build());

            case 14:
                primaryOption = Option.builder()
                        .section(0)
                        .direction(RIGHT)
                        .build();
                alternativeOption = Option.builder()
                        .section(15)
                        .direction(LEFT)
                        .build();

                return Optional.of(handleChoice(primaryOption, alternativeOption, endSection, blockedSections));
            case 15:
                return Optional.empty();
            default:
                throw new IllegalArgumentException("Unknown current section");
        }
    }

    private Option handleChoice(Option primaryOption, Option alternativeOption, int endSection, List<Integer> blockedSections) throws NoPathException {
        if (endSection == alternativeOption.getSection()) {
            if (blockedSections.contains(alternativeOption.getSection())) {
                throw new NoPathException();
            }

            return alternativeOption;
        }

        if (blockedSections.contains(primaryOption.getSection())) {
            if (blockedSections.contains(alternativeOption.getSection())) {
                throw new NoPathException();
            }

            return alternativeOption;
        }

        return primaryOption;
    }

    private Option handleChoice(Option onlyOption, List<Integer> blockedSections) throws NoPathException {
        if (blockedSections.contains(onlyOption.getSection())) {
            throw new NoPathException();
        }

        return onlyOption;
    }
}
