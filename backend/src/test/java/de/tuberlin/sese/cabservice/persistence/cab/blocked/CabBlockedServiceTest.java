package de.tuberlin.sese.cabservice.persistence.cab.blocked;

import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Optional;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class CabBlockedServiceTest {

    @Autowired
    private CabBlockedService service;

    @MockBean
    private CabBlockedRepo blockedRepo;

    @MockBean
    private CabRepo cabRepo;

    @MockBean
    private CabLocationService locationService;

    @Captor
    private ArgumentCaptor<CabBlockedEntity> entityCaptor;

    @Test
    public void shouldBlockCab() {
        when(cabRepo.findById(3L)).thenReturn(Optional.of(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build()));

        service.setBlocked(3L, true);

        verify(cabRepo).findById(3L);
        verify(blockedRepo).findById(3L);
        verify(blockedRepo).save(entityCaptor.capture());
        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(blockedRepo);

        assertThat(entityCaptor.getValue().getCabId()).isEqualTo(3);
    }

    @Test
    public void shouldBlockAlreadyBlockedCabWithoutException() {
        when(cabRepo.findById(3L)).thenReturn(Optional.of(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build()));

        service.setBlocked(3L, true);
        service.setBlocked(3L, true);
    }

    @Test
    public void shouldUnblockBlockedCabWithoutException() {
        when(cabRepo.findById(3L)).thenReturn(Optional.of(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build()));

        service.setBlocked(3L, true);
        service.setBlocked(3L, false);
    }

    @Test
    public void shouldUnblockAlreadyUnblockedCabWithoutException() {
        when(cabRepo.findById(3L)).thenReturn(Optional.of(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build()));

        service.setBlocked(3L, false);
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionOnUnknownCabIdWhenBlocking() {
        assertThatThrownBy(() -> service.setBlocked(3L, true))
                .isExactlyInstanceOf(UnknownCabIdException.class);
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionOnUnknownCabIdWhenUnblocking() {
        assertThatThrownBy(() -> service.setBlocked(3L, false))
                .isExactlyInstanceOf(UnknownCabIdException.class);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCabId() {
        assertThatThrownBy(() -> service.setBlocked(null, true))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingBlocked() {
        when(cabRepo.findById(3L)).thenReturn(Optional.of(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build()));

        assertThatThrownBy(() -> service.setBlocked(3L, null))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldGetBlockedSections() {
        when(blockedRepo.findAll()).thenReturn(asList(
                CabBlockedEntity.builder()
                        .cabId(1L)
                        .build(),
                CabBlockedEntity.builder()
                        .cabId(2L)
                        .build(),
                CabBlockedEntity.builder()
                        .cabId(3L)
                        .build(),
                CabBlockedEntity.builder()
                        .cabId(4L)
                        .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(locationService.getCabLocation(2L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(2L)
                .section(8)
                .build()));

        when(locationService.getCabLocation(3L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(3L)
                .section(4)
                .build()));

        when(locationService.getCabLocation(4L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(4L)
                .section(9)
                .build()));

        assertThat(service.getBlockedSections())
                .containsExactlyInAnyOrder(2, 4, 8, 9);
    }

    @Test
    public void shouldReturnBlockedStateIfCabIsBlocked() {
        when(cabRepo.findById(3L)).thenReturn(Optional.of(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build()));

        when(blockedRepo.findById(3L)).thenReturn(Optional.of(CabBlockedEntity.builder()
                .cabId(3L)
                .build()));

        assertThat(service.isBlocked(3L)).isTrue();
    }

    @Test
    public void shouldReturnBlockedStateIfCabIsNotBlocked() {
        when(cabRepo.findById(3L)).thenReturn(Optional.of(CabEntity.builder()
                .id(3L)
                .name("Some Cab Name")
                .build()));

        when(blockedRepo.findById(3L)).thenReturn(Optional.empty());

        assertThat(service.isBlocked(3L)).isFalse();
    }
}
