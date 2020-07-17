package de.tuberlin.sese.cabservice.persistence.reset;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class ResetController {

    private final ResetService service;

    @PostMapping("/bookr/softResetBackend")
    public void softResetBackend() {
        service.resetAllRepos();
    }

    @PostMapping("/bookr/hardResetBackend")
    public void hardResetBackend() {
        service.resetSpringBootApp();
    }

}
